open Printf
open Log
open Lwt
open Stripe_t

type trial_end = [ `Now | `Later of Util_time.t ]

(* Override this to raise your own exception *)
let payment_required_handler : (string -> exn) ref =
  ref (fun errmsg -> Failure errmsg)

let payment_required errmsg =
  raise (!payment_required_handler errmsg)

let string_of_trial_end (x : trial_end) =
  match x with
  | `Now -> "now"
  | `Later t -> Util_time.As_unixtime.to_string t

let trial_end_of_string s : trial_end =
  match s with
  | "now" -> `Now
  | s -> `Later (Util_time.As_unixtime.of_string s)

type billing_cycle_anchor = [ `Unchanged ]

let string_of_billing_cycle_anchor (x : billing_cycle_anchor) =
  match x with
  | `Unchanged -> "unchanged"

let prod_secret_key = ref (None : string option)
let test_secret_key = ref (None : string option)

let secret_key for_test =
  if for_test
  then !test_secret_key
  else !prod_secret_key

(* to_string functions *)
let string s : string = s
let int i = string_of_int i
let decimal2 f = sprintf "%.2f" f
let bool b = string_of_bool b
let timestamp x = sprintf "%.0f" (Util_time.to_float x)
let trial_end_s = string_of_trial_end
let billing_cycle_anchor_s = string_of_billing_cycle_anchor

(* Usage: opt int "age" (Some 123) *)
let opt to_string name o =
  match o with
  | None -> []
  | Some x -> [name, to_string x]

(* Usage: req int "age" 123 *)
let req to_string name x =
  [name, to_string x]

(* Usage: list int "bingo" [2; 14; 16; 44] *)
let list to_string parent_name l =
  List.map (fun (name, v) ->
    (sprintf "%s[%s]" parent_name name, to_string v)
  ) l

let make_url path (param: (string * string) list list) =
  Uri.make
    ~scheme: "https"
    ~host: "api.stripe.com"
    ~path
    ~query: (List.map (fun (k, v) -> (k, [v])) (List.flatten param))
    ()

let make_auth_header ~for_test =
  match secret_key for_test with
  | None -> failwith "Stripe API: missing secret key"
  | Some key ->
      let user_pass = key ^ ":" (* no password *) in
      let v = BatString.trim (Util_enc.base64_encode user_pass) in
      ["Authorization", "Basic " ^ v]

let get ~for_test path param =
  Perf.time "stripe" (fun () ->
    Util_http_client.get
      ~headers: (make_auth_header ~for_test)
      (make_url path param)
  )

let post ~for_test path param =
  Perf.time "stripe" (fun () ->
    Util_http_client.post
      ~headers: (make_auth_header ~for_test)
      (make_url path param)
  )

let delete ~for_test path param =
  Perf.time "stripe" (fun () ->
    Util_http_client.delete
      ~headers: (make_auth_header ~for_test)
      (make_url path param)
  )

let with_error (status, headers, body) f =
  let error =
    try (Stripe_j.error_response_of_string body).error
    with e ->
      logf `Error "Unparsable error response body: %s" body;
      {
        type_ = "unknown";
        message =
          sprintf "Malformed response. HTTP status %s"
            (Cohttp.Code.string_of_status status);
        code = None;
        param = None
      }
  in
  f error

let report_error function_name x =
  with_error x (fun e ->
    logf `Error "Stripe API call %s failed:\n%s"
      function_name
      (Yojson.Safe.prettify (Stripe_j.string_of_error e));
    failwith (sprintf "Stripe API call %s failed" function_name)
  )

type card_details = {
  number: string;
    (* The card number, as a string without any separators. *)
  exp_month: int;
    (* Two digit number representing the card's expiration month. *)
  exp_year: int;
    (* Two or four digit number representing the card's expiration year. *)
  cvc: string option;
    (* Card security code. *)
  name: string option;
    (* Cardholder's full name. *)
  address_line1: string option;
  address_line2: string option;
  address_city: string option;
  address_zip: string option;
  address_state: string option;
  address_country: string option;
}

(*
   There are two ways of adding a card for a customer.

   In a client-server application, normally the client obtains a card token
   from Stripe directly, then passes this token to the app server
   which can register it with that customer and access some of the card's
   details, but not the full card number for instance.

   The other method consists in providing the full card details directly.
   This is used for testing and by applications that don't mind the liability
   that comes with handling full card details.
*)
type new_card =
  | Card_token of string
  | Card_details of card_details

let wrap root_field_name l =
  List.map (fun (k, v) -> sprintf "%s[%s]" root_field_name k, v) l

let new_card field_name = function
  | None -> []
  | Some (Card_token token) -> ["card", token]
  | Some (Card_details x) -> wrap "card" (List.flatten [
      req string "number" x.number;
      req int "exp_month" x.exp_month;
      req int "exp_year" x.exp_year;
      opt string "cvc" x.cvc;
      opt string "name" x.name;
      opt string "address_line1" x.address_line1;
      opt string "address_line2" x.address_line2;
      opt string "address_city" x.address_city;
      opt string "address_zip" x.address_zip;
      opt string "address_state" x.address_state;
      opt string "address_country" x.address_country;
    ])

(** Check whether the given string corresponds to a removed object. If
 *  it does, returns None; otherwise, uses the given parsing function to
 *  return a parsed object.
 *)
let check_delete of_string str =
  let { del_deleted } = Stripe_j.deleted_of_string str in
  match del_deleted with
  | Some true -> None
  | _         -> Some (of_string str)

(*
   Generic function to fetch a list of items.

   Multiple pages are fetched if needed to reach the maximum length (limit)
   or the end of the list.
*)
let rec get_list
    ~for_test
    ~path
    ~read_item
    ~get_item_id
    ?starting_after
    ?ending_before
    ?(limit = 1000)
    ?(param = [])
    () : _ list option Lwt.t =

  get ~for_test path (param @ [
    opt string "starting_after" starting_after;
    opt string "ending_before" ending_before;
    req int "limit" limit;
  ])
  >>= function
  | (`OK, headers, body) ->
      let x = Stripe_j.stripe_list_of_string read_item body in
      let l = x.list_data in
      let max_remaining =
        if l = [] || not x.list_has_more then 0
        else
          max 0 (limit - List.length l)
      in
      if max_remaining = 0 then
        return (Some l)
      else (
        assert (l <> []);
        let last = get_item_id (BatList.last l) in
        get_list
          ~for_test
          ~path
          ~read_item
          ~get_item_id
          ~starting_after: last
          ?ending_before
          ~limit: max_remaining
          ()
        >>= function
        | None -> failwith "Invalid list page request returned 404 Not Found"
        | Some tail ->
            return (Some (l @ tail))
      )

  | (`Not_found, headers, body) ->
      return None

  | x ->
      report_error "get_list" x


let create_customer
  ~for_test
  ?account_balance
  ?card
  ?coupon
  ?description
  ?email
  ?(metadata = [])
  ?plan
  ?quantity
  ?trial_end
  () : simple_customer Lwt.t =

  post ~for_test "/v1/customers" [
    opt int "account_balance" account_balance;
    new_card "card" card;
    opt string "coupon" coupon;
    opt string "description" description;
    opt string "email" email;
    list string "metadata" metadata;
    opt string "plan" plan;
    opt int "quantity" quantity;
    opt trial_end_s "trial_end" trial_end;
  ]
  >>= function
  | (`OK, headers, body) ->
      return (Stripe_j.simple_customer_of_string body)
  | (`Payment_required, header, body) ->
      payment_required "Invalid card"
  | x ->
      report_error "create_customer" x


let get_customer
    ~for_test
    ~customer_id
    () : simple_customer option Lwt.t =
  get ~for_test ("/v1/customers/"  ^ Util_url.encode customer_id) []
  >>= function
  | (`OK, headers, body) ->
      return (check_delete Stripe_j.simple_customer_of_string body)
  | (`Not_found, headers, body) ->
      return None
  | x ->
      report_error "get_customer" x


let update_customer
  ~for_test
  ?account_balance
  ?card
  ?coupon
  ?default_source
  ?description
  ?email
  ?(metadata = [])
  ~customer_id
  () : simple_customer option Lwt.t =

  post ~for_test ("/v1/customers/"  ^ Util_url.encode customer_id) [
    opt int "account_balance" account_balance;
    new_card "card" card;
    opt string "coupon" coupon;
    opt string "default_source" default_source;
    opt string "description" description;
    opt string "email" email;
    list string "metadata" metadata;
  ]
  >>= function
  | (`OK, headers, body) ->
      return (Some (Stripe_j.simple_customer_of_string body))
  | (`Not_found, headers, body) ->
      return None
  | (`Payment_required, header, body) ->
      payment_required "Invalid card"
  | x ->
      report_error "update_customer" x


let delete_customer
    ~for_test
    ~customer_id
    () : deleted_customer option Lwt.t =

  delete ~for_test ("/v1/customers/"  ^ Util_url.encode customer_id) []
  >>= function
  | (`OK, headers, body) ->
      return (Some (Stripe_j.deleted_customer_of_string body))
  | (`Not_found, headers, body) ->
      return None
  | x ->
      report_error "delete_customer" x


let list_customers
  ~for_test
  ?created_after  (* exclusive *)
  ?created_before (* exclusive *)
  ?created_from   (* inclusive *)
  ?created_until  (* inclusive *)
  ?ending_before
  ?limit
  ?starting_after
  () : simple_customer list Lwt.t =

  let path = "/v1/customers" in
  let param = [
    opt timestamp "created[gt]" created_after;
    opt timestamp "created[gte]" created_from;
    opt timestamp "created[lt]" created_before;
    opt timestamp "created[lte]" created_until;
  ] in
  get_list
    ~for_test
    ~path
    ~read_item: Stripe_j.read_simple_customer
    ~get_item_id: (fun cus -> cus.cus_id)
    ?starting_after
    ?ending_before
    ?limit
    ~param
    ()
  >>= function
  | None -> failwith "Call to get a page of customers returned 404 Not Found"
  | Some l -> return l

let create_subscription
  ~for_test
  ~plan
  ?coupon
  ?trial_end
  ?card
  ?quantity
  ?application_fee_percent
  ?(metadata = [])
  ~customer_id
  () : subscription Lwt.t =

  post ~for_test ("/v1/customers/" ^ Util_url.encode customer_id ^
                  "/subscriptions") [
    req string "plan" plan;
    opt string "coupon" coupon;
    opt trial_end_s "trial_end" trial_end;
    new_card "card" card;
    opt int "quantity" quantity;
    opt decimal2 "application_fee_percent" application_fee_percent;
    list string "metadata" metadata;
  ]
  >>= function
  | (`OK, headers, body) -> return (Stripe_j.subscription_of_string body)
  | (`Payment_required, header, body) ->
      payment_required "Invalid card"
  | x -> report_error "subscribe" x

let get_subscription
    ~for_test
    ~customer_id
    ~subscription_id
    () : subscription option Lwt.t =
  get ~for_test ("/v1/customers/" ^ Util_url.encode customer_id
       ^ "/subscriptions/" ^ Util_url.encode subscription_id) []
  >>= function
  | (`OK, headers, body) ->
      return (check_delete Stripe_j.subscription_of_string body)
  | (`Not_found, headers, body) ->
      return None
  | x ->
      report_error "get_subscription" x


let list_subscriptions
  ~for_test
  ~customer_id
  ?ending_before
  ?limit
  ?starting_after
  () : subscription list option Lwt.t =

  let path =
    "/v1/customers/" ^ Util_url.encode customer_id ^ "/subscriptions"
  in
  get_list
    ~for_test
    ~path
    ~read_item: Stripe_j.read_subscription
    ~get_item_id: (fun sub -> sub.sub_id)
    ?starting_after
    ?ending_before
    ?limit
    ()

let update_subscription
  ~for_test
  ?plan
  ?coupon
  ?prorate
  ?trial_end
    (* if trial_set is not set, a new whole trial period will start again
       if the status was `Active *)
  ?card
  ?quantity
  ?application_fee_percent
  ?(metadata = [])
  ?billing_cycle_anchor
    (* undocumented feature.
       From Stripe support:

       On 03/20/2015 10:57 AM, Nelson Elhage wrote:
> Hi Martin,
>
> I believe Jim explained on-thread what the rationale for the current
> behavior is. However, I talked to the engineers, and we do actually
> support a (currently) undocumented feature that I think will result in
> the behavior you want.
>
> When you upgrade from a free to a paid plan, if you pass the option
> "billing_cycle_anchor = 'unchanged'", that should result in the billing
> cycle remaining the same, and thus start billing at the end of the
> current month.
    *)

  ~customer_id
  ~subscription_id
  () : subscription option Lwt.t =

  post ~for_test ("/v1/customers/" ^ Util_url.encode customer_id ^
                  "/subscriptions/" ^ Util_url.encode subscription_id) [
    opt string "plan" plan;
    opt string "coupon" coupon;
    opt bool "prorate" prorate;
    opt trial_end_s "trial_end" trial_end;
    new_card "card" card;
    opt int "quantity" quantity;
    opt decimal2 "application_fee_percent" application_fee_percent;
    list string "metadata" metadata;
    opt billing_cycle_anchor_s "billing_cycle_anchor" billing_cycle_anchor;
  ] >>= function
  | (`OK, headers, body) ->
      return (Some (Stripe_j.subscription_of_string body))
  | (`Not_found, headers, body) ->
      return None
  | (`Payment_required, header, body) ->
      payment_required "Invalid card"
  | x ->
      report_error "update_subscription" x

let cancel_subscription
  ~for_test
  ?at_period_end
  ~customer_id
  ~subscription_id
  () : subscription option Lwt.t =

  delete ~for_test ("/v1/customers/" ^ Util_url.encode customer_id ^
                    "/subscriptions/" ^ Util_url.encode subscription_id) [
    opt bool "at_period_end" at_period_end;
  ] >>= function
  | (`OK, headers, body) ->
      return (Some (Stripe_j.subscription_of_string body))
  | (`Not_found, headers, body) ->
      return None
  | x ->
      report_error "cancel_subscription" x


let create_card
  ~for_test
  ~customer_id
  ~card
  () : simple_card option Lwt.t =

  post ~for_test ("/v1/customers/" ^ Util_url.encode customer_id ^ "/cards") [
    new_card "card" (Some card);
  ]
  >>= function
  | (`OK, headers, body) ->
      return (Some (Stripe_j.simple_card_of_string body))
  | (`Not_found, headers, body) ->
      return None
  | (`Payment_required, header, body) ->
      payment_required "Invalid card"
  | x ->
      report_error "create_card" x


let get_card
  ~for_test
  ~customer_id
  ~card_id
  () : simple_card option Lwt.t =

  get ~for_test("/v1/customers/" ^ Util_url.encode customer_id ^
                "/cards/" ^ Util_url.encode card_id) []
  >>= function
  | (`OK, headers, body) ->
      return (check_delete Stripe_j.simple_card_of_string body)
  | (`Not_found, headers, body) ->
      return None
  | (`Payment_required, header, body) ->
      payment_required "Invalid card"
  | x ->
      report_error "create_card" x

let delete_card
  ~for_test
  ~customer_id
  ~card_id
  () : deleted_card option Lwt.t =

  delete ~for_test ("/v1/customers/" ^ Util_url.encode customer_id ^
                    "/cards/" ^ Util_url.encode card_id) []
  >>= function
  | (`OK, headers, body) ->
      return (Some (Stripe_j.deleted_card_of_string body))
  | (`Not_found, headers, body) ->
      return None
  | x ->
      report_error "create_card" x

let list_cards
  ~for_test
  ~customer_id
  ?starting_after
  ?ending_before
  ?limit
  () : simple_card list option Lwt.t =

  let path = "/v1/customers/" ^ Util_url.encode customer_id ^ "/cards" in
  get_list
    ~for_test
    ~path
    ~read_item: Stripe_j.read_simple_card
    ~get_item_id: (fun card -> card.card_id)
    ?starting_after
    ?ending_before
    ?limit
    ()

let create_charge
  ~for_test
  ~amount
    (*
       A positive integer in the smallest currency unit (e.g 100 cents
       to charge $1.00, or 1 to charge ¥1, a 0-decimal currency)
       representing how much to charge the card. The minimum amount is
       $0.50 (or equivalent in charge currency).
    *)
  ?(currency = "usd")
    (*
       3-letter ISO code for currency.
    *)
  ?customer_id
    (*
       optional, either customer or source is required.
       The ID of an existing customer that will be charged in this
       request.
    *)
  ?source
    (*
       optional, either source or customer is required.
       A payment source to be charged, such as a credit card. If you
       also pass a customer ID, the source must be the ID of a source
       belonging to the customer. Otherwise, if you do not pass a
       customer ID, the source you provide must either be a token,
       like the ones returned by Stripe.js, or a dictionary containing
       a user's credit card details, with the options described
       below. Although not all information is required, the extra info
       helps prevent fraud.
        child attributes
    *)
  ?description
    (*
       An arbitrary string which you can attach to a charge object. It
       is displayed when in the web interface alongside the
       charge. Note that if you use Stripe to send automatic email
       receipts to your customers, your receipt emails will include
       the description of the charge(s) that they are describing.
    *)
  ?(metadata = [])
    (*
       A set of key/value pairs that you can attach to a charge
       object. It can be useful for storing additional information
       about the customer in a structured format. It's often a good
       idea to store an email address in metadata for tracking later.
    *)
  ?capture
    (*
       optional, default is true
       Whether or not to immediately capture the charge. When false,
       the charge issues an authorization (or pre-authorization), and
       will need to be captured later. Uncaptured charges expire in 7
       days. For more information, see authorizing charges and
       settling later.
    *)
  ?statement_descriptor
    (*
       An arbitrary string to be displayed on your customer's credit
       card statement. This may be up to 22 characters. As an example,
       if your website is RunClub and the item you're charging for is
       a race ticket, you may want to specify a statement_descriptor
       of RunClub 5K race ticket. The statement description may not
       include '<', '>', '"', '\'',
       and will appear on your customer's
       statement in capital letters. Non-ASCII characters are
       automatically stripped. While most banks display this
       information consistently, some may display it incorrectly or
       not at all.
    *)

  ?receipt_email
    (*
       The email address to send this charge's receipt to. The receipt
       will not be sent until the charge is paid. If this charge is
       for a customer, the email address specified here will override
       the customer's email address. Receipts will not be sent for
       test mode charges. If receipt_email is specified for a charge
       in live mode, a receipt will be sent regardless of your email
       settings.
    *)
  ?application_fee
    (*
       A fee in cents that will be applied to the charge and
       transferred to the application owner's Stripe account. The
       request must be made with an OAuth key in order to take an
       application fee. For more information, see the application fees
       documentation.
    *)
  ?(shipping = [])
    (*
       Shipping information for the charge. Helps prevent fraud on
       charges for physical goods.
    *)

  () : charge Lwt.t =

  (match customer_id, source with
   | None, None -> invalid_arg "create_charge: missing customer_id or source"
   | _ -> ()
  );

  post ~for_test "/v1/charges" [
    req int "amount" amount;
    req string "currency" currency;
    opt string "customer" customer_id;
    opt string "source" source;
    opt string "description" description;
    list string "metadata" metadata;
    opt bool "capture" capture;
    opt string "statement_descriptor" statement_descriptor;
    opt string "receipt_email" receipt_email;
    opt int "application_fee" application_fee;
    list string "shipping" shipping;
  ]
  >>= function
  | (`OK, headers, body) -> return (Stripe_j.charge_of_string body)
  | x -> report_error "create_charge" x

let capture_charge
  ~for_test
  ~charge_id
  ?amount
    (*
       The amount to capture, which must be less than or equal to the
       original amount. Any additional amount will be automatically
       refunded.
    *)

  ?application_fee
    (*
       An application fee to add on to this charge. Can only be used with
       Stripe Connect.
    *)

  ?receipt_email
    (*
       The email address to send this charge’s receipt to. This will
       override the previously-specified email address for this charge,
       if one was set. Receipts will not be sent in test mode.
    *)

  ?statement_descriptor
    (*
       An arbitrary string to be displayed on your customer’s credit
       card statement. This may be up to 22 characters. As an example,
       if your website is RunClub and the item you’re charging for is
       a race ticket, you may want to specify a statement_descriptor
       of RunClub 5K race ticket. The statement description may not
       include '<' '>' '\'' '\"' characters, and will appear on your
       customer’s statement in capital letters. Non-ASCII characters are
       automatically stripped. Updating this value will overwrite the
       previous statement descriptor of this charge. While most banks
       display this information consistently, some may display it
       incorrectly or not at all.
    *)
  () : charge Lwt.t =

  post ~for_test ("/v1/charges/" ^ charge_id ^ "/capture") [
    opt int "amount" amount;
    opt int "application_fee" application_fee;
    opt string "receipt_email" receipt_email;
    opt string "statement_descriptor" statement_descriptor;
  ]
  >>= function
  | (`OK, headers, body) -> return (Stripe_j.charge_of_string body)
  | x -> report_error "capture_charge" x


let create_invoice
  ~for_test
  ~customer_id
  ?application_fee
    (*
       A fee in cents that will be applied to the invoice
       and transferred to the application owner’s Stripe account.
       The request must be made with an OAuth key or
       the Stripe-Account header in order to take an application fee.
       For more information, see the application fees documentation.
    *)
  ?description
  ?(metadata = [])
  ?statement_descriptor
    (*
       Extra information about a charge for the customer’s
       credit card statement.
    *)
  ?subscription_id
    (*
       The ID of the subscription to invoice. If not set, the created
       invoice will include all pending invoice items for the
       customer. If set, the created invoice will exclude pending
       invoice items that pertain to other subscriptions.
    *)
  ?tax_percent
    (*
       The percent tax rate applied to the invoice,
       represented as a decimal number.
    *)

  () : invoice Lwt.t =

  post ~for_test "/v1/invoices" [
    req string "customer" customer_id;
    opt int "application_fee" application_fee;
    opt string "description" description;
    list string "metadata" metadata;
    opt string "statement_descriptor" statement_descriptor;
    opt string "subscription" subscription_id;
    opt decimal2 "tax_percent" tax_percent;
  ]
  >>= function
  | (`OK, headers, body) -> return (Stripe_j.invoice_of_string body)
  | x -> report_error "create_invoice" x


let list_invoices
  ~for_test
  ~customer_id
  ?starting_after
  ?ending_before
  ?limit
  () : invoice list option Lwt.t =

  let path = "/v1/invoices" in
  get_list
    ~for_test
    ~path
    ~read_item: Stripe_j.read_invoice
    ~get_item_id: (fun invoice -> invoice.inv_id)
    ?starting_after
    ?ending_before
    ?limit
    ~param: [["customer", customer_id]]
    ()


let create_invoice_item
  ~for_test
  ~customer_id
  ~amount
  ?(currency="usd")
  ?invoice_id
  ?subscription_id
  ?description
  ?discountable
  ?(metadata=[])
  (): invoice_item Lwt.t =

  post ~for_test "/v1/invoiceitems" [
    req  string "customer"     customer_id;
    req  int    "amount"       amount;
    req  string "currency"     currency;
    opt  string "invoice"      invoice_id;
    opt  string "subscription" subscription_id;
    opt  string "description"  description;
    opt  bool   "discountable" discountable;
    list string "metadata"     metadata;
  ]
  >>= function
  | (`OK, _headers, body) -> return (Stripe_j.invoice_item_of_string body)
  | x -> report_error "create_invoice_item" x


let get_event_raw
    ~for_test
    ~event_id
    () : 'a option Lwt.t =

  get ~for_test ("/v1/events/" ^ Util_url.encode event_id) []
  >>= function
  | (`OK, headers, body) ->
      return (Some body)
  | (`Not_found, headers, body) ->
      return None
  | x ->
      report_error "get_event" x

let get_event
    read_item
    ~for_test
    ~event_id
    () : 'a option Lwt.t =
  get_event_raw
    ~for_test
    ~event_id
    () >>= function
  | Some s -> return (Some (Stripe_j.event_of_string read_item s))
  | None -> return None

let get_event_data read ~for_test ~event_id =
  get_event read ~for_test ~event_id ()

let get_subscription_event ~for_test ~event_id =
  get_event_data Stripe_j.read_subscription ~for_test ~event_id

let get_invoice_event ~for_test ~event_id =
  get_event_data Stripe_j.read_invoice ~for_test ~event_id
