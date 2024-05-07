{ pkgs, ... }: {
  # output .mbsyncrc file
  programs.mbsync.enable = true;
  programs.msmtp.enable = true;

  accounts.email = {
    accounts.icloud = {
      address = "ldangelo@mac.com";
      imap.host = "imap.mail.me.com";
      userName = "ldangelo@mac.com";
      passwordCommand = "pmqv-lupe-ifxg-kxja";
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
  };
}
