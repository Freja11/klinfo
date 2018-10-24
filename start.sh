#!/bin/sh
NODE=klinfo@$(hostname)
APP=klinfo_app
cd `dirname $0`
exec erl -smp auto +P 134217727 +K true +A 64 -pa $PWD/_build/default/lib/*/ebin -boot start_sasl -s lager -s $APP $@ -config config/sys.config -sname $NODE -setcookie chat_cookie
