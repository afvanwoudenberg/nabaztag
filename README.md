# Nabaztag
This is a server written in Prolog for the [nabaztag:tag](https://en.wikipedia.org/wiki/Nabaztag) bunny. It passes events like a button press or a voice recording to the https://ifttt.com website where it can be hooked up to other services through recipes. The server also exposes an API that can be called from the https://ifttt.com website or other services (or even a simple curl command) to make the bunny execute certain commands.

## Configuration
Before you can start this server you'll need to add some keys to the `config.pl` file. Specifically, you'll need to set the bunny's serial number (MAC address) and the webhook key of your ifttt.com account. You'll need to include the MAC address on multiple lines. If you own more than one nabaztag, you may repeat the last four lines for your second (and third etc) bunny. 

## Running the server
To run this server you'll need to have SWI-Prolog installed. Consult the `nabaztag.pl` file and on the prompt type:

`server_start.`

This will start the server. To stop the server you may type:

`server_stop.`

## Events
The bunny sends events to the ifttt.com website where they can be caught using webhooks. Make sure you have included the right key in the `config.pl` file. The events that the server produces have the following names:

- `<SN>_wakeup`: The bunny with serial number SN completes loading its bootcode.
- `<SN>_click`: The user clicks the button on the bunny's head.
- `<SN>_dblclick`: The user presses the button on the bunny's head quickly twice.
- `<SN>_interrupt`: The user presses the button while the bunny is executing a command.
- `<SN>_earsmove`: The ears of the bunny are adjusted. The call to the ifttt also includes the position of the ears.
- `<SN>_record_click`: A message is recorded using a simple click. An URL that points to the recorded message is also sent to ifttt.
- `<SN>_record_dblclick`: A message is recorded using a double click. Again, an URL to the recorded message is sent to ifttt.
- `<SN>_<RFID>`: A Zstamps with a <RFID> key is read.

Every event is accompanied with a second event called `nabaztag`. This can be used as a catch-all event to, for instance, enable logging to a google spreadsheet. It can also be used to find out the RFID IDs of Zstamps.

## API
The server defines the following API calls.

### Text To Speech

`HTTP POST http://host:port/vl/api/tts.jsp?sn=<SN>&lang=<LANG>&text=<TEXT>`

where `<SN>` is the bunny's serial number, `<LANG>` is a language code, such as `en` (default), and `<TEXT>` is the text to pronounce.

### Play MP3 from URL

`HTTP POST http://host:port/vl/api/play.jsp?sn=<SN>&url=<URL>`

where `<SN>` is the bunny's serial number and `<URL>` points to the web address where the MP3 is located.

### Play stream from URL

`HTTP POST http://host:port/vl/api/stream.jsp?sn=<SN>&url=<URL>`

where `<SN>` is the bunny's serial number and `<URL>` points to the web address where the stream is located.

###  Reboot

`HTTP POST http://host:port/vl/api/reboot.jsp?sn=<SN>`

where `<SN>` is the bunny's serial number

### Play choreography

`HTTP POST http://host:port/vl/api/chor.jsp?sn=<SN>&chor=<CHOR>&url=<URL>`

where `<SN>` is the bunny's serial number, `<CHOR>` is the name of a built-in choreography, and `<URL>` points to the web address where the choreography file is located. Either `<CHOR>` or `<URL>` needs to be provided. Currently, valid values for `<CHOR>` are: `rainbow`, `slider`, `disco`, `larson`, and `blink`.

## Ambient settings

`HTTP POST http://host:port/vl/api/ambient.jsp?sn=<SN>&chor=<CHOR>&leftear=<LE>&rightear=<RE>&nose=<NOSE>&weather=<WEATHER>&market=<MARKET>&traffic=<TRAFFIC>&messages=<MESSAGES>&airquality=<AIRQUALITY>`

where `<SN>` is the bunny's serial number, `<LE>` is the position of the left ear, `<RE>` is the position of the right ear, `<NOSE>` has a value of 0-2, `<WEATHER>` is one of (`none`,`cloudy`,`smog`,`rain`,`snow`,`storm`), `<MARKET>` is one of (`none`,`highdown`,`mediumdown`,`littledown`,`stable`,`littleup`,`mediumup`,`highup`), `<TRAFFIC>` is one of (`none`,`100`,`75`,`50`,`25`,`12`,`8`,`4`), `<MESSAGES>` is one of (`none`,`0`,`1`,`2`,`3`), and `<AIRQUALITY>` is one of (`none`,`good`,`medium`,`low`). 

All parameters except `<SN>` are optional. A value of `none` deletes a previous setting.
