-ifndef(soap_srv_hrl).
-define(soap_srv_hrl, defined).

-include_lib("alley_dto/include/adto.hrl").

-type soap_sms_type() ::
	latin |
	arabic_with_arabic_numbers |
	arabic_with_latin_numbers.

-record(send_sms_req, {
	customer_id :: binary(),
	user_name	:: binary(),
	password	:: binary(),
	originator 	:: #addr{},
	recipients	:: [#addr{}],
	text		:: binary(),
	type		:: soap_sms_type(),
	def_date	:: binary() | undefined,
	flash		:: boolean()
}).

-record(send_service_sms_req, {
		customer_id 	:: binary(),
		user_name 		:: binary(),
		password 		:: binary(),
		originator 		::  #addr{},
		recipients		:: [#addr{}],
		service_name 	:: binary(),
		service_url 	:: binary(),
		type 			:: soap_sms_type(),
		def_date 		:: binary() | undefined,
		flash 			:: boolean
}).

-record(pworker, {
	id			:: term(),
	timestamp	:: integer(),
	from		:: {pid(), term()}
}).

-record(presponse, {
	id			:: term(),
	timestamp	:: integer(),
	response	:: term()
}).

-endif. % soap_srv.hrl
