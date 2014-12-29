%% The data of WHOIS responses
-record(response, {
        raw :: binary(),
        domain :: domain,
        db_date :: binary(),
        terms :: binary()
       }). 

%% The domain data extracted from WHOIS responses
-record(domain, {
        name :: binary(),
        registrar :: binary(),
        referral_url :: binary(),
        name_servers :: list(),
        status :: list(),
        updated_date :: binary(),
        creation_date :: binary(),
        expiration_date :: binary()
       }).

%% The TLD data
-record(tld, {
        tld :: binary(),
        domain :: binary(),
        subdomains = [] :: list(),
        available = unknown
       }).
