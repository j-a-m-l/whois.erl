%% The data of WHOIS responses
%% -record(response, {
%%         raw :: binary(),
%%         whois :: whois,
%%         db_date :: binary(),
%%         terms :: binary()
%%        }). 

%% The domain data extracted from WHOIS responses
-record(whois, {
        domain :: binary(),
        registrar :: binary(),
        referral_url :: binary(),
        name_servers = [] :: list(),
        status = [] :: list(),
        update_date :: binary(),
        creation_date :: binary(),
        expiration_date :: binary(),
        %% Inferred fields
        available = unknown :: atom()
       }).

%% The TLD data TODO
-record(tld, {
        tld :: binary(),
        domain :: binary(),
        subdomains = [] :: list(),
        available = unknown
       }).
