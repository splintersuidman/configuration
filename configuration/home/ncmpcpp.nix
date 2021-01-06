{ pkgs, config, lib, ... }:
let mainColour = "blue";
in {
  programs.ncmpcpp = {
    enable = true;
    settings = {
      mpd_port = config.services.mopidy.config.mpd.port;
      lyrics_fetchers = lib.concatStringsSep "," [
        "lyricwiki"
        "genius"
        "azlyrics"
        "sing365"
        "lyricsmania"
        "metrolyrics"
        "justsomelyrics"
        "jahlyrics"
        "plyrics"
        "tekstowo"
        "internet"
      ];
      lyrics_directory = config.xdg.dataHome + "/lyrics";
      allow_for_physical_item_deletion = true;
      user_interface = "alternative";
      progressbar_look = "─•─";

      colors_enabled = true;
      empty_tag_color = "default";
      header_window_color = "default";
      volume_color = "default";
      state_line_color = "default";
      state_flags_color = "default:b";
      main_window_color = "default";
      color1 = "white";
      color2 = mainColour;
      progressbar_color = "black:b";
      progressbar_elapsed_color = "${mainColour}:b";
      statusbar_color = "default";
      statusbar_time_color = "default:b";
      player_state_color = "default:b";
      alternative_ui_separator_color = "black:b";
      window_border_color = mainColour;
      active_window_border = "white";

      song_list_format = "$0{%a - }{%t}|{$8%f$9}$R{$3(%l)$9}";
      song_status_format = ''{{%a{ "%b"{ (%y)}} - }{%t}}|{%f}'';
      song_library_format = "{%n - }{%t}|{%f}";
      alternative_header_first_line_format =
        "$b$1$aqqu$/a$9 $8{%t}|{%f}$9 $1$atqq$/a$9$/b";
      alternative_header_second_line_format =
        "{{$3$b%a$/b$9}{ - $8$b%b$/b$9}{ $0(%y)$9}}|{%D}";
      current_item_prefix = "$3$r$b";
      current_item_suffix = "$/b$/r$9";
      current_item_inactive_column_prefix = "$b";
      current_item_inactive_column_suffix = "$/b";
      now_playing_prefix = "$b";
      now_playing_suffix = "$/b";
      browser_playlist_prefix = "$0playlist$9 ";
      selected_item_prefix = "$3";
      selected_item_suffix = "$9";
      modified_item_prefix = "$3> $9";
      song_window_title_format = "{%a - }{%t}|{%f}";
      song_columns_list_format =
        "(20)[]{a} (6f)[]{NE} (50)[${mainColour}]{t|f:Title} (20)[white]{b} (7f)[]{l}";
    };

    bindings = lib.concatLists (lib.mapAttrsToList (key: commands:
      builtins.map (command: {
        key = key;
        command = command;
      }) commands) {
        "!" = [ "toggle_separators_between_albums" ];
        "#" = [ "toggle_bitrate_visibility" ];
        "+" = [ "volume_up" ];
        "," = [ "previous_found_item" ];
        "-" = [ "volume_down" ];
        "." = [ "next_found_item" ];
        "/" = [ "seek_forward" "start_searching" "find" "find_item_forward" ];
        "1" = [ "show_playlist" ];
        "2" = [ "show_browser" "change_browse_mode" ];
        "3" = [ "show_search_engine" "reset_search_engine" ];
        "4" = [ "show_media_library" "toggle_media_library_columns_mode" ];
        "5" = [ "show_playlist_editor" ];
        "6" = [ "show_tag_editor" ];
        "7" = [ "show_outputs" ];
        "8" = [ "show_visualizer" ];
        ":" = [ "execute_command" ];
        "=" = [ "show_clock" ];
        "?" = [ "seek_backward" "find" "find_item_backward" ];
        "@" = [ "show_server_info" ];
        A = [ "add" ];
        B = [ "select_album" ];
        C = [ "crop_playlist" "crop_main_playlist" ];
        E = [ "jump_to_tag_editor" ];
        F = [ "fetch_lyrics_in_background" ];
        G = [ "jump_to_browser" "jump_to_playlist_editor" ];
        I = [ "show_artist_info" ];
        L = [ "show_lyrics" ];
        M = [ "move_selected_items_to" ];
        P = [ "toggle_display_mode" ];
        R = [ "toggle_consume" ];
        S = [ "save_playlist" ];
        T = [ "toggle_add_mode" ];
        U = [ "toggle_playing_song_centering" ];
        V = [ "remove_selection" ];
        X = [ "set_crossfade" ];
        Y = [ "toggle_replay_gain_mode" ];
        Z = [ "shuffle" ];
        "[" = [ "scroll_up_album" ];
        "\\\\" = [ "toggle_interface" ];
        "]" = [ "scroll_down_album" ];
        "`" = [ "toggle_library_tag_type" "refetch_lyrics" "add_random_items" ];
        a = [ "add_selected_items" ];
        alt-l = [ "toggle_fetching_lyrics_in_background" ];
        backspace = [ "jump_to_parent_directory" "replay_song" ];
        c = [ "clear_playlist" "clear_main_playlist" ];
        ctrl-_ = [ "select_found_items" ];
        ctrl-f = [ "apply_filter" ];
        ctrl-h = [ "replay_song" ];
        ctrl-l = [ "toggle_lyrics_fetcher" ];
        ctrl-p = [ "set_selected_items_priority" ];
        ctrl-r = [ "reverse_playlist" ];
        ctrl-s = [
          "sort_playlist"
          "toggle_browser_sort_mode"
          "toggle_media_library_sort_mode"
        ];
        ctrl-v = [ "select_range" ];
        d = [
          "delete_playlist_items"
          "delete_browser_items"
          "delete_stored_playlist"
        ];
        e = [
          "edit_song"
          "edit_library_tag"
          "edit_library_album"
          "edit_directory_name"
          "edit_playlist_name"
          "edit_lyrics"
        ];
        enter = [
          "enter_directory"
          "toggle_output"
          "run_action"
          "play_item"
          "slave_screen"
        ];
        f1 = [ "show_help" ];
        g = [ "move_home" ];
        h = [
          "previous_column"
          "master_screen"
          "volume_down"
          "jump_to_parent_directory"
        ];
        i = [ "show_song_info" ];
        insert = [ "select_item" ];
        j = [ "scroll_down" ];
        k = [ "scroll_up" ];
        l = [
          "enter_directory"
          "run_action"
          "play_item"
          "next_column"
          "slave_screen"
          "volume_up"
        ];
        m = [ "move_sort_order_up" "move_selected_items_up" ];
        mouse = [ "mouse_event" ];
        n = [ "next" "move_sort_order_down" "move_selected_items_down" ];
        o = [ "jump_to_playing_song" ];
        p = [ "pause" ];
        page_down = [ "page_down" ];
        page_up = [ "page_up" ];
        q = [ "quit" ];
        r = [ "toggle_repeat" ];
        s = [ "stop" ];
        shift-g = [ "move_end" ];
        shift-j = [[ "select_item" "scroll_down" ]];
        shift-k = [[ "select_item" "scroll_up" ]];
        shift-n = [ "previous" ];
        shift-tab = [ "previous_screen" ];
        space = [
          "add_item_to_playlist"
          "toggle_lyrics_update_on_song_change"
          "toggle_visualization_type"
        ];
        tab = [ "next_screen" ];
        u = [ "update_database" ];
        v = [ "reverse_selection" ];
        w = [ "toggle_find_mode" ];
        x = [ "toggle_crossfade" ];
        y = [ "save_tag_changes" "toggle_single" ];
        z = [ "toggle_random" ];
        "{" = [ "scroll_up_artist" ];
        "|" = [ "toggle_mouse" ];
        "}" = [ "scroll_down_artist" ];
        "~" = [ "jump_to_media_library" ];
      });
  };
}
