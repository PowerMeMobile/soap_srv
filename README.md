It is a SOAP interface for [OpenAlley] smpp gateway.

## Features ##

- send outgoing sms (MT)
- receive incoming sms (MO)
- retrieve sms status
- transport: soap1.1, soap1.2, http get, http post

## Requirements ##

Install, configure and run the next apps using this [guide]:

1. Erlang r15b01
2. RabbitMQ
3. MongoDB
4. [Kelly] OpenAlley SMPP middleware
5. [Just] SMPP gateway
6. SMPPSim gateway

## Installation and launching ##

<pre>
git clone https://github.com/PowerMeMobile/soap_srv.git
cd ./soap_srv
make && make console
</pre>

Type in another shell to send sms:

<pre>
./test/soap_stress_test
</pre>

If all is OK, you'll see `Message sucessfully sent` in the soap_srv console window.

#### WSDL ####
Browse at http://127.0.0.1:8888/bmsgw/soap/messenger.asmx?wsdl
#### Settings ####

Edit ./rel/soap_srv/releases/1/sys.conf to configure:

1. RabbitMQ host and port
2. Soap srv http port
3. Soap srv http logging
4. Soap srv pdu logging
5. Soap srv common logging

### Tests ###

Start soap_srv before any test.

#### Load test ####

Edit ./test/soap_stress_test macroses at the top of the file to change:

1. Request type: `req_type` macros (soap 1.1, soap 1.2, http get, http post)
2. Number of parallel processes: `proc_num` macros
3. Number of requests each process should do: `req_num` macros
4. Soap srv port and host: `host` and `port` macroses respectively
5. Other customer's account properties

#### Functional test ####

To run all test:

<pre>
cd ./test
./test
</pre>

To run specified test:

<pre>
cd ./test
./test *TestName*
</pre>

Name of tests you can find at ./test/soap_test.config.

#### Test MO messages ####

Create subscription in kelly for existing user. Type in kelly console:

    rr("lib/k_mailbox-1/include/application.hrl").
    Sub = #k_mb_k1api_incoming_sms_sub{
        id = <<"1">>,
        customer_id = <<"a3ddc34a-1793-11e2-9602-00269e42f7a5">>,
        user_id = <<"undefined">>,
        priority = 1,
        queue_name = <<"pmm.alley.soap.incoming.sms">>,
        dest_addr = {addr, <<"375296660003">>,1,1,undefined},
        notify_url = <<"http://localhost:4444/test">>,
        callback_data = <<>>
    }.
    k_mailbox:register_subscription(Sub).

Start soap_srv in development mode, to start echo mo srv:

<pre>
make develop
</pre>

Send MO message (from kelly dir):

<pre>
./rel/files/send_mo_msgs -c 1 -b 'HelloPMM!' -d 375296660003
</pre>

Check for result in kelly console:

    Item successfully delivered [<<"cae046ae-04c6-11e3-8cad-001d0947ec73">>]

Check for result in soap_srv console:

    12:49:20.971 [info] Got InboundSms: {k1api_sms_notification_request_dto,<<>>,{1376,473760,0},{addr,<<"375296660003">>,1,1,undefined},<<"cae046ae-04c6-11e3-8cad-001d0947ec73">>,<<"HelloPMM!">>,{addr,<<"375295152966">>,1,0,undefined},<<"http://localhost:4444/test">>}
    12:49:20.980 [debug] Delivered: "http://localhost:4444/test?Sender=375295152966&Destination=375296660003&MessageType=1&MessageText=HelloPMM!&MessageTextRaw=48656C6C6F504D4D21&CurrentPart=0&NumberOfParts=0"
    12:49:20.980 [info] ack

## Support and help ##

Feel free to open [issues] and [pull requests] on github.

[OpenAlley]: http://www.powermemobile.com/PressRelease-OpenAlley
[kelly]: https://github.com/PowerMeMobile/kelly
[guide]: https://github.com/PowerMeMobile/kelly#readme
[just]: https://github.com/PowerMeMobile/just_mini_rel
[issues]: https://github.com/PowerMeMobile/soap_srv/issues
[pull requests]: https://github.com/PowerMeMobile/soap_srv/pulls
[todo]: https://github.com/PowerMeMobile/soap_srv#readme
