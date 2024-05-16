{ pkgs, environment, ... }: {
  # output .mbsyncrc file
  programs.mbsync.enable = true;
  programs.msmtp.enable = true;

  accounts.email = {
    accounts.icloud = {
      address = "ldangelo@mac.com";
      imap.host = "imap.mail.me.com";
      userName = "ldangelo@mac.com";
      passwordCommand = "op item get mbsync-icloud --fields password";
      mbsync = {
        enable = true;
        create = "maildir";
      };
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
       extraConfig.account = {
         AuthMechs = "XOAUTH2";
       };
     };
     msmtp.enable = true;
     primary = false;
     realName = "Leo A. D'Angelo";
     smtp = {
       host = "localhost";
       port = 1465;
     };
    };
  };
}
