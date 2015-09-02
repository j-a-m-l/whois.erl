whois.erl
=========
This project performs [WHOIS](https://en.wikipedia.org/wiki/Whois) requests.

* [RFC 3912](https://tools.ietf.org/html/rfc3912)

Notes
-----

This project is standing by while I developed others. Anyway, if you want to contribute, I will help you ;).


Requirements
============
This project is self-contained and it doesn't have any requirement.

It is being developed and tested on Erlang/OTP 17.


Installation
============


API
===

whois.erl is an OTP application, with its own supervisor.
TODO? That supervisor uses an ETS table `whois_server` for keeping the state of the requests and responses.

TODO async...
```erlang
TlRecord = whois:lookup("domain.com").
```

```erlang
TldRecord = whois:lookup(<<"sub.domain.com">>).
```

TldRecord
---------
```erlang
TldRecord
```

Server
------
Instead of using `whois:lookup/1`, which involves preparing the module on every call, it is possible to run a server, and query it directly: TODO
```erlang
Pid = whois_server:start(Options),

receive
  {ok, TldRecords} ->
    other:action(TldRecords),
    loop();
  {error, Reason} ->
    Reason
end,

Pid ! {whois, "domain.com"},
%% Pid ! {whois, ["domain1.com", "domain2.com"]},
whois_server:start:stop().
```


Data
====
You can update it, indepently of the rhythm of this project, using TODO.


Contributing
============

Development workflow
--------------------
1. Create an issue
TODO

Tools
-----

Tests
-----


Authors
=======
Juan Antonio Martín Lucas (https://github.com/j-a-m-l)


License
=======
This package is licensed under the MIT license.
