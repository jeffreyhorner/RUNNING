#!/bin/bash
GARMIN_EPO_LOCATION=/media/hornerj/GARMIN/GARMIN/REMOTESW/EPO.BIN
curl -H "Garmin-Client-Name: CoreService" -H "Content-Type: application/octet-stream" --data-binary @garmin-postdata http://omt.garmin.com/Rce/ProtobufApi/EphemerisService/GetEphemerisData | ./strip_checksum.pl > $GARMIN_EPO_LOCATION
