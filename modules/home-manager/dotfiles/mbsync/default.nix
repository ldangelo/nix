{ pkgs, environment, ... }: {
  # output .mbsyncrc file
  programs.mbsync = {
    enable = true;
    package = pkgs.isync-oauth2;
    extraConfig = ''
      # Keeps timestamp mased message sorting intactt
      CopyArrivalDate yes
      SyncState *
    '';
  };

  programs.msmtp.enable = true;
  programs.notmuch = {
    enable = true;

    new.tags = [ "new" ];
    hooks = {
      preNew = "mbsync -a";
      postNew = "afew -t -n -vv -C ~/.config/notmuch/default/config";
    };
  };
  programs.afew = {
    enable = true;

    extraConfig = ''

      [SentMailsFilter]
      sent_tag=+sent;

      [SpamFilter]
      [KillThreadsFilter]
      [ListMailsFilter]
      [ArchiveSentMailsFilter]

      [Filter.0]
      query = from:fortiumpartners.com
      tags = +important
      message = Message from Fortium

      [Filter.1]
      query = from:hchb.com
      tags = +important;+currentclient;+hchb
      message = Message from HCHB

      [Filter.2]
      query = from:musicmaster.com
      tags  = +important;+currentclient;+musicmaster
      message = Mesage from MusicMaster

      [InboxFilter]
    '';
  };

  accounts = {

    email = {
      accounts.icloud = {
        address = "ldangelo@mac.com";
        imap.host = "imap.mail.me.com";
        userName = "ldangelo@mac.com";
        passwordCommand = "op item get mbsync-icloud --fields password";
        mbsync = {
          enable = true;
          create = "maildir";
        };
        notmuch.enable = true;
        msmtp = {
          enable = true;
          extraConfig = {
            tls = "on";
            tls_starttls = "on";
            port = "587";
            tls_certcheck = "off";
          };
        };
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
        msmtp = {
          enable = true;
          extraConfig = {
            auth = "oauthbearer";
            tls = "on";
            tls_starttls = "on";
            port = "587";
            tls_certcheck = "off";
          };
        };
         notmuch.enable = true;
        primary = false;
        realName = "Leo A. D'Angelo";
        smtp = {
          host = "smtp.gmail.com";
#          port = 1465;
        };
      };
    };
  };
}
