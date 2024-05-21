{ pkgs, environment, ... }: {
  # output .mbsyncrc file
  # programs.mbsync = {
  #   enable = true;
  #   package = pkgs.isync-oauth2;
  #   extraConfig = ''
  #     # Keeps timestamp mased message sorting intactt
  #     CopyArrivalDate yes
  #     SyncState *
  #     Sync All
  #   '';
  # };
  programs.offlineimap = {
    enable = true;
    extraConfig.general = {
      accounts = "icloud";
      metadata = "~/Maildir/.offlineimap";
    };
  };
  programs.msmtp.enable = true;
  programs.neomutt.enable = true;
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
        imap = {
          host = "imap.mail.me.com";
          tls.enable = true;
          tls.useStartTls = true;
        };
        userName = "ldangelo@mac.com";
        # Use the keychain to keep from having to type the op password in... it sucks!
        passwordCommand = "op item get mbsync-icloud --fields password";

        flavor = "plain";
        offlineimap = { enable = true; };
        # mbsync = {
        #   enable = true;
        #   create = "maildir";
        #   patterns = [
        #     "INBOX"
        #     "Trash"

        #   ];
        # };
        notmuch.enable = true;
        neomutt.enable = true;
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
        imap = {
          host = "imap.gmail.com";
          tls.enable = true;
          tls.useStartTls = true;
        };
        offlineimap.enable = true;
        userName = "leo.dangelo@fortiumpartners.com";
        passwordCommand = "pizauth show fortium";
        # mbsync = {
        #   enable = true;
        #   create = "both";
        #   expunge = "both";
        #   extraConfig.account = { AuthMechs = "XOAUTH2"; };
        # };
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
