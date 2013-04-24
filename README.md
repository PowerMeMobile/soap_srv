This is app make you able to send sms via [OpenAlley] smpp gateway using soap.
soap_srv is already support mt sms messages, but work is in progress.
See [todo] list and todo comments in source files for more information about feature work.

## Requirements ##

Install, configure and run next apps using this [guide]:

1. Erlang r15b01
2. RabbitMQ
3. MongoDB
4. [Kelly] OpenAlley SMPP middleware
5. [Just] SMPP gateway
6. SmppSim gateway

## Installation and run ##

    git clone https://github.com/PowerMeMobile/soap_srv.git

    cd ./soap_srv

    make && make console

Type in another shell to send sms:

    ./test/soap_stress_test

If all is OK, you'll see `Message sucessfully sent` in the soap_srv console window.

### Settings ###

Edit `./rel/soap_srv/releases/1/sys.conf` to setup:

1. RabbitMQ host and port
2. Soap srv http port
3. Soap srv http logging
4. Soap srv pdu loggin
5. Soap srv common loggin

### Test tool ###

Edit `./test/soap_stress_test` macroses at the top of the file to change:

1. Request type: `req_type` macros (soap 1.1, soap 1.2, http get, http post)
2. Nuber of parallel proceses: `proc_num` macros
3. Request number each process should do: `req_num` macros
4. Soap srv port and host: `host` and `port` macroses respectively
5. Othrer properties about customer's account

## Support and help ##

Feel free to open [issues] and [pull requests] on github.

## TODO ##

1. Support for mo sms (incoming) messages
2. RabbitMQ public confirms for mt sms messages

[OpenAlley]: http://www.powermemobile.com/PressRelease-OpenAlley
[kelly]: https://github.com/PowerMeMobile/kelly
[guide]: https://github.com/PowerMeMobile/kelly#readme
[just]: https://github.com/PowerMeMobile/just_mini_rel
[issues]: https://github.com/PowerMeMobile/soap_srv/issues
[pull requests]: https://github.com/PowerMeMobile/soap_srv/pulls
[todo]: https://github.com/PowerMeMobile/soap_srv#readme