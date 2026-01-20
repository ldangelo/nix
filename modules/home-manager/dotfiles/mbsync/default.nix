{ pkgs, environment, ... }: {
  # output .mbsyncrc file
   programs.mbsync = {
     enable = true;
     package = pkgs.isync-oauth2;  # TODO: isync-oauth2 doesn't exist in nixpkgs, may need custom overlay for OAuth2 support
     extraConfig = ''
      # Keeps timestamp mased message sorting intactt
      CopyArrivalDate yes
      SyncState *
 '';
   };

  programs.msmtp.enable = true;
  programs.notmuch = {
    enable = true;

    new.tags = [
      "new"
    ];
    hooks = {
      preNew = "mbsync -all";
      postNew = "afew --tag --new";
    };
  };

  accounts = {

    email = {
      accounts.icloud = {
        address = "ldangelo@mac.com";
        imap.host = "imap.mail.me.com";
        userName = "ldangelo@mac.com";
#        passwordCommand = "op item get mbsync-icloud --fields password";
        passwordCommand = "cat /run/secrets/mac_mail_key";
        mbsync = {
          enable = true;
          create = "both";
          expunge = "both";
        };
        notmuch.enable = true;
        msmtp.enable = true;
        primary = true;
        realName = "Leo A. DAngelo";
        signature = {
          text = ''
            -LeoD

            Leo A. DAngelo
            ldangelo@mac.com
          '';
          showSignature = "append";
        };
        smtp = { host = "smtp.mail.me.com"; };
      };
      accounts.fortium = {
        address = "leo.dangelo@fortiumpartners.com";
        imap.host = "imap.gmail.com";
        imap.tls.enable = true;
        userName = "leo.dangelo@fortiumpartners.com";
        passwordCommand = "pizauth show fortium";
        mbsync = {
          enable = true;
          create = "both";
          expunge = "both";
          extraConfig.account = { AuthMechs = "XOAUTH2"; };
        };
        msmtp.enable = true;
        notmuch.enable = true;
        primary = false;
        realName = "Leo A. D'Angelo";
        smtp = {
          host = "localhost";
          port = 1465;
        };
      };
    };
  };
}
