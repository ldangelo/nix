{ config, lib, pkgs, ... }:

{
  services.pizauth = {
    enable = true;
    accounts = {
      name = "fortium";
    authUri = "https://accounts.google.com/o/oauth2/auth";
    tokenIri = "https://oauth2.googleapis.com/token";
    clientId = "406964657835-aq8lmia8j95dhl1a2bvharmfk3t1hgqj.apps.googleusercontent.com";
    clientSecret = "cat /var/run/secrets/pizauthsecret";
    scopes = ["https://mail.google.com/"];
    };
  };
}
