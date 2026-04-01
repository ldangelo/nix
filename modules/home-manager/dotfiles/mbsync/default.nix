{ pkgs, environment, ... }: {
  # output .mbsyncrc file
   programs.mbsync = {
     enable = true;
     package = pkgs.isync-oauth2;  # Custom overlay with OAuth2 support via cyrus-sasl-xoauth2
     extraConfig = ''
      # Keeps timestamp-based message sorting intact
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
      preNew = "mbsync icloud & mbsync fortium & mbsync curantis & wait";
      postNew = "~/Development/crush-mail/crush-mail tag && ~/Development/crush-mail/crush-mail move";
    };
  };

  accounts = {

    email = {
      maildirBasePath = "Maildir";

      accounts.icloud = {
        address = "ldangelo@mac.com";
        imap.host = "imap.mail.me.com";
        userName = "ldangelo";
        passwordCommand = "cat /run/secrets/mac_mail_key";
        mbsync = {
          enable = true;
          create = "both";
          expunge = "both";
          extraConfig.account = { PipelineDepth = 50; };
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
        smtp = {
          host = "smtp.mail.me.com";
          port = 587;
        };
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
          extraConfig.account = { AuthMechs = "XOAUTH2"; PipelineDepth = 50; };
        };
        msmtp.enable = true;
        msmtp.extraConfig = {
          auth = "xoauth2";
          tls_starttls = "on";
        };
        notmuch.enable = true;
        primary = false;
        realName = "Leo A. D'Angelo";
        signature = {
          text = ''
            Leo D'Angelo
            Partner
            Fortium Partners
            Mobile: (972) 979-0116
          '';
          showSignature = "append";
        };
        smtp = {
          host = "smtp.gmail.com";
          port = 587;
          tls.enable = true;
        };
      };

      accounts.curantis = {
        address = "leo.dangelo@curantissolutions.com";
        imap.host = "outlook.office365.com";
        imap.tls.enable = true;
        userName = "leo.dangelo@curantissolutions.com";
        passwordCommand = "pizauth show curantis";
        mbsync = {
          enable = true;
          create = "both";
          expunge = "none";
          extraConfig.account = { AuthMechs = "XOAUTH2"; PipelineDepth = 50; };
          patterns = [ "*" "!Conversation History" "!Conversation History/*" "!Sync Issues" "!Sync Issues/*" ];
        };
        msmtp.enable = true;
        msmtp.extraConfig = {
          auth = "xoauth2";
          tls_starttls = "on";
        };
        notmuch.enable = true;
        primary = false;
        realName = "Leo D'Angelo";
        signature = {
          text = ''
            Leo D'Angelo
            Chief Technology Officer
            Curantis Solutions
            3939 Belt Line Rd. | Suite 350 | Addison, TX 75001
            Mobile: (972) 979-0116
          '';
          showSignature = "append";
        };
        smtp = {
          host = "smtp-mail.outlook.com";
          port = 587;
          tls.enable = true;
        };
      };
    };
  };
}
