{pkgs, ...}: let
	yazi-plugins = pkgs.fetchFromGitHub {
		owner = "yazi-rs";
		repo = "plugins";
		rev = "de339764c315059014245a8f5245298a2d29f7e3";
		hash = "sha256-60xxQZlXsKs7F/TJdRkhgZpTXas6rUvJUb0rvwZIozo=";
	};
in {
	programs.yazi = {
		enable = true;
		enableZshIntegration = true;
		shellWrapperName = "y";

		# Preview dependencies
		package = pkgs.yazi;
		extraPackages = with pkgs; [
			ffmpegthumbnailer  # video thumbnails
			poppler-utils      # PDF previews
			_7zz               # archive previews
		];

		settings = {
			mgr = {
				show_hidden = true;
			};
			preview = {
				max_width = 1000;
				max_height = 1000;
			};
			plugin = {
				prepend_fetchers = [
					{ id = "git"; name = "*"; run = "git"; }
					{ id = "git"; name = "*/"; run = "git"; }
				];
			};
		};

		plugins = {
			chmod = "${yazi-plugins}/chmod.yazi";
			full-border = "${yazi-plugins}/full-border.yazi";
			toggle-pane = "${yazi-plugins}/toggle-pane.yazi";
			smart-filter = "${yazi-plugins}/smart-filter.yazi";
			jump-to-char = "${yazi-plugins}/jump-to-char.yazi";
			git = "${yazi-plugins}/git.yazi";
			diff = "${yazi-plugins}/diff.yazi";
			smart-enter = "${yazi-plugins}/smart-enter.yazi";
			smart-paste = "${yazi-plugins}/smart-paste.yazi";
			mactag = "${yazi-plugins}/mactag.yazi";
			starship = pkgs.fetchFromGitHub {
				owner = "Rolv-Apneseth";
				repo = "starship.yazi";
				rev = "eca186171c5f2011ce62712f95f699308251c749";
				sha256 = "sha256-xcz2+zepICZ3ji0Hm0SSUBSaEpabWUrIdG7JmxUl/ts=";
			};
		};

		initLua = ''
			require("full-border"):setup()
			require("starship"):setup()
			require("git"):setup()
		'';

		keymap = {
			mgr.prepend_keymap = [
				{
					on = "T";
					run = "plugin toggle-pane max-preview";
					desc = "Maximize or restore the preview pane";
				}
				{
					on = ["c" "m"];
					run = "plugin chmod";
					desc = "Chmod on selected files";
				}
				{
					on = "f";
					run = "plugin smart-filter";
					desc = "Smart filter";
				}
				{
					on = "F";
					run = "plugin jump-to-char";
					desc = "Jump to char";
				}
				{
					on = "l";
					run = "plugin smart-enter";
					desc = "Open file or enter directory";
				}
				{
					on = "p";
					run = "plugin smart-paste";
					desc = "Smart paste";
				}
				{
					on = ["d" "f"];
					run = "plugin diff";
					desc = "Diff selected file";
				}
				{
					on = ["c" "t"];
					run = "plugin mactag";
					desc = "macOS tag management";
				}
			];
		};
	};
}
