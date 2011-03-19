unit vte;
interface
uses glib2, pango, gdk2, gtk2, gdk2pixbuf;

{$DEFINE VTEVERSION_17}  // remove this define if compiling for libvte9 < 0.17.6
{$DEFINE VTEVERSION_20}  // remove this define if compiling for libvte9 < 0.20
{$DEFINE VTEVERSION_26}  // remove this define if compiling for libvte9 < 0.26

{$H+}
{$linklib vte}
const
    External_library = 'vte';

{
 libvte is in package libvte9
 on installation run a script using
 locate libvte|grep .so
 then
 ln -s /usr/lib/libvte.so.9 /usr/lib/libvte.so
 if required
}

type
 pid_t = Integer;
 PVteTerminal = PGtkWidget;
 TVteTerminalEraseBinding = (VTE_ERASE_AUTO,VTE_ERASE_ASCII_BACKSPACE,VTE_ERASE_ASCII_DELETE,VTE_ERASE_DELETE_SEQUENCE);
 TVteTerminalCursorBlinkMode = (VTE_CURSOR_BLINK_SYSTEM, VTE_CURSOR_BLINK_ON, VTE_CURSOR_BLINK_OFF);
 TVteTerminalAntiAlias = (VTE_ANTI_ALIAS_USE_DEFAULT,VTE_ANTI_ALIAS_FORCE_ENABLE,VTE_ANTI_ALIAS_FORCE_DISABLE);
 TVteTerminalCursorShape = (VTE_CURSOR_SHAPE_BLOCK, VTE_CURSOR_SHAPE_IBEAM, VTE_CURSOR_SHAPE_UNDERLINE);

 PVtePty = Pointer;

 TVtePtyFlags = (VTE_PTY_NO_LASTLOG  = 1 shl 0,
                 VTE_PTY_NO_UTMP     = 1 shl 1,
                 VTE_PTY_NO_WTMP     = 1 shl 2,
                 VTE_PTY_NO_HELPER   = 1 shl 3,
                 VTE_PTY_NO_FALLBACK = 1 shl 4,
                 VTE_PTY_DEFAULT     = 0);

 TGSpawnFlags = (G_SPAWN_LEAVE_DESCRIPTORS_OPEN = 1 shl 0,
                 G_SPAWN_DO_NOT_REAP_CHILD      = 1 shl 1,
                 // look for argv[0] in the path i.e. use execvp()
                 G_SPAWN_SEARCH_PATH            = 1 shl 2,
                 // Dump output to /dev/null
                 G_SPAWN_STDOUT_TO_DEV_NULL     = 1 shl 3,
                 G_SPAWN_STDERR_TO_DEV_NULL     = 1 shl 4,
                 G_SPAWN_CHILD_INHERITS_STDIN   = 1 shl 5,
                 G_SPAWN_FILE_AND_ARGV_ZERO     = 1 shl 6 );

  PGRegex = Pointer;


    { You can get by with just these two functions.  }
    function vte_terminal_new:PGtkWidget;cdecl;external External_library name 'vte_terminal_new';

    {void        vte_terminal_im_append_menuitems
                                            (VteTerminal *terminal,
                                             GtkMenuShell *menushell);
}

    function vte_terminal_fork_command(terminal:PVteTerminal; command:Pchar; argv:PPchar; envv:PPchar; directory:Pchar; 
               lastlog:gboolean; utmp:gboolean; wtmp:gboolean):pid_t;cdecl;external External_library name 'vte_terminal_fork_command';
    //NOTE: vte_terminal_fork_command has been deprecated since version 0.26 and should not be used in newly-written code. Use vte_terminal_fork_command_full()

    { Users of libzvt may find this useful.  }
(* Const before type ignored *)
    function vte_terminal_forkpty(terminal:PVteTerminal; envv:PPchar; directory:Pchar; lastlog:gboolean; utmp:gboolean; 
               wtmp:gboolean):pid_t;cdecl;external External_library name 'vte_terminal_forkpty';

    { Send data to the terminal to display, or to the terminal's forked command
     * to handle in some way.  If it's 'cat', they should be the same.  }
(* Const before type ignored *)
    procedure vte_terminal_feed(terminal:PVteTerminal; data:Pchar; length:glong);cdecl;external External_library name 'vte_terminal_feed';

(* Const before type ignored *)
    procedure vte_terminal_feed_child(terminal:PVteTerminal; text:Pchar; length:glong);cdecl;external External_library name 'vte_terminal_feed_child';

(* Const before type ignored *)
    procedure vte_terminal_feed_child_binary(terminal:PVteTerminal; data:Pchar; length:glong);cdecl;external External_library name 'vte_terminal_feed_child_binary';

    { Copy currently-selected text to the clipboard, or from the clipboard to
     * the terminal.  }
    procedure vte_terminal_copy_clipboard(terminal:PVteTerminal);cdecl;external External_library name 'vte_terminal_copy_clipboard';

    procedure vte_terminal_paste_clipboard(terminal:PVteTerminal);cdecl;external External_library name 'vte_terminal_paste_clipboard';

    procedure vte_terminal_copy_primary(terminal:PVteTerminal);cdecl;external External_library name 'vte_terminal_copy_primary';

    procedure vte_terminal_paste_primary(terminal:PVteTerminal);cdecl;external External_library name 'vte_terminal_paste_primary';

    { simple manipulation of selection  }
    procedure vte_terminal_select_all(terminal:PVteTerminal);cdecl;external External_library name 'vte_terminal_select_all';

    procedure vte_terminal_select_none(terminal:PVteTerminal);cdecl;external External_library name 'vte_terminal_select_none';

    { Set the terminal's size.  }
    procedure vte_terminal_set_size(terminal:PVteTerminal; columns:glong; rows:glong);cdecl;external External_library name 'vte_terminal_set_size';

    { Set various on-off settings.  }
    procedure vte_terminal_set_audible_bell(terminal:PVteTerminal; is_audible:gboolean);cdecl;external External_library name 'vte_terminal_set_audible_bell';

    function vte_terminal_get_audible_bell(terminal:PVteTerminal):gboolean;cdecl;external External_library name 'vte_terminal_get_audible_bell';

    procedure vte_terminal_set_visible_bell(terminal:PVteTerminal; is_visible:gboolean);cdecl;external External_library name 'vte_terminal_set_visible_bell';

    function vte_terminal_get_visible_bell(terminal:PVteTerminal):gboolean;cdecl;external External_library name 'vte_terminal_get_visible_bell';

    procedure vte_terminal_set_scroll_background(terminal:PVteTerminal; scroll:gboolean);cdecl;external External_library name 'vte_terminal_set_scroll_background';

    procedure vte_terminal_set_scroll_on_output(terminal:PVteTerminal; scroll:gboolean);cdecl;external External_library name 'vte_terminal_set_scroll_on_output';

    procedure vte_terminal_set_scroll_on_keystroke(terminal:PVteTerminal; scroll:gboolean);cdecl;external External_library name 'vte_terminal_set_scroll_on_keystroke';

    { Set the color scheme.  }
(* Const before type ignored *)
    procedure vte_terminal_set_color_dim(terminal:PVteTerminal; dim:PGdkColor);cdecl;external External_library name 'vte_terminal_set_color_dim';

(* Const before type ignored *)
    procedure vte_terminal_set_color_bold(terminal:PVteTerminal; bold:PGdkColor);cdecl;external External_library name 'vte_terminal_set_color_bold';

(* Const before type ignored *)
    procedure vte_terminal_set_color_foreground(terminal:PVteTerminal; foreground:PGdkColor);cdecl;external External_library name 'vte_terminal_set_color_foreground';

(* Const before type ignored *)
    procedure vte_terminal_set_color_background(terminal:PVteTerminal; background:PGdkColor);cdecl;external External_library name 'vte_terminal_set_color_background';

(* Const before type ignored *)
    procedure vte_terminal_set_color_cursor(terminal:PVteTerminal; cursor_background:PGdkColor);cdecl;external External_library name 'vte_terminal_set_color_cursor';

(* Const before type ignored *)
    procedure vte_terminal_set_color_highlight(terminal:PVteTerminal; highlight_background:PGdkColor);cdecl;external External_library name 'vte_terminal_set_color_highlight';

(* Const before type ignored *)
(* Const before type ignored *)
(* Const before type ignored *)
    procedure vte_terminal_set_colors(terminal:PVteTerminal; foreground:PGdkColor; background:PGdkColor; palette:PGdkColor; palette_size:glong);cdecl;external External_library name 'vte_terminal_set_colors';

    procedure vte_terminal_set_default_colors(terminal:PVteTerminal);cdecl;external External_library name 'vte_terminal_set_default_colors';

    { Background effects.  }
    procedure vte_terminal_set_background_image(terminal:PVteTerminal; image:PGdkPixbuf);cdecl;external External_library name 'vte_terminal_set_background_image';

(* Const before type ignored *)
    procedure vte_terminal_set_background_image_file(terminal:PVteTerminal; path:Pchar);cdecl;external External_library name 'vte_terminal_set_background_image_file';

(* Const before type ignored *)
    procedure vte_terminal_set_background_tint_color(terminal:PVteTerminal; color:PGdkColor);cdecl;external External_library name 'vte_terminal_set_background_tint_color';

    procedure vte_terminal_set_background_saturation(terminal:PVteTerminal; saturation:double);cdecl;external External_library name 'vte_terminal_set_background_saturation';

    procedure vte_terminal_set_background_transparent(terminal:PVteTerminal; transparent:gboolean);cdecl;external External_library name 'vte_terminal_set_background_transparent';

    procedure vte_terminal_set_opacity(terminal:PVteTerminal; opacity:guint16);cdecl;external External_library name 'vte_terminal_set_opacity';

    { Set whether or not the cursor blinks.  }
    procedure vte_terminal_set_cursor_blinks(terminal:PVteTerminal; blink:gboolean);cdecl;external External_library name 'vte_terminal_set_cursor_blinks';

    { Set the number of scrollback lines, above or at an internal minimum.  }
    procedure vte_terminal_set_scrollback_lines(terminal:PVteTerminal; lines:glong);cdecl;external External_library name 'vte_terminal_set_scrollback_lines';

    { Append the input method menu items to a given shell.  }
    procedure vte_terminal_im_append_menuitems(terminal:PVteTerminal; menushell:PGtkMenuShell);cdecl;external External_library name 'vte_terminal_im_append_menuitems';

    { Set or retrieve the current font.  }
(* Const before type ignored *)
    procedure vte_terminal_set_font(terminal:PVteTerminal; font_desc:PPangoFontDescription);cdecl;external External_library name 'vte_terminal_set_font';

(* Const before type ignored *)
    procedure vte_terminal_set_font_full(terminal:PVteTerminal; font_desc:PPangoFontDescription; antialias:TVteTerminalAntiAlias);cdecl;external External_library name 'vte_terminal_set_font_full';

(* Const before type ignored *)
    procedure vte_terminal_set_font_from_string(terminal:PVteTerminal; name:Pchar);cdecl;external External_library name 'vte_terminal_set_font_from_string';

(* Const before type ignored *)
    procedure vte_terminal_set_font_from_string_full(terminal:PVteTerminal; name:Pchar; antialias:TVteTerminalAntiAlias);cdecl;external External_library name 'vte_terminal_set_font_from_string_full';

(* Const before type ignored *)
    function vte_terminal_get_font(terminal:PVteTerminal):PPangoFontDescription;cdecl;external External_library name 'vte_terminal_get_font';

    function vte_terminal_get_using_xft(terminal:PVteTerminal):gboolean;cdecl;external External_library name 'vte_terminal_get_using_xft';

    procedure vte_terminal_set_allow_bold(terminal:PVteTerminal; allow_bold:gboolean);cdecl;external External_library name 'vte_terminal_set_allow_bold';

    function vte_terminal_get_allow_bold(terminal:PVteTerminal):gboolean;cdecl;external External_library name 'vte_terminal_get_allow_bold';

    { Check if the terminal is the current selection owner.  }
    function vte_terminal_get_has_selection(terminal:PVteTerminal):gboolean;cdecl;external External_library name 'vte_terminal_get_has_selection';

    { Set the list of word chars, optionally using hyphens to specify ranges
     * (to get a hyphen, place it first), and check if a character is in the
     * range.  }
(* Const before type ignored *)
    procedure vte_terminal_set_word_chars(terminal:PVteTerminal; spec:Pchar);cdecl;external External_library name 'vte_terminal_set_word_chars';

    function vte_terminal_is_word_char(terminal:PVteTerminal; c:gunichar):gboolean;cdecl;external External_library name 'vte_terminal_is_word_char';

    { Set what happens when the user strikes backspace or delete.  }
    procedure vte_terminal_set_backspace_binding(terminal:PVteTerminal; binding:TVteTerminalEraseBinding);cdecl;external External_library name 'vte_terminal_set_backspace_binding';

    procedure vte_terminal_set_delete_binding(terminal:PVteTerminal; binding:TVteTerminalEraseBinding);cdecl;external External_library name 'vte_terminal_set_delete_binding';

    { Manipulate the autohide setting.  }
    procedure vte_terminal_set_mouse_autohide(terminal:PVteTerminal; setting:gboolean);cdecl;external External_library name 'vte_terminal_set_mouse_autohide';

    function vte_terminal_get_mouse_autohide(terminal:PVteTerminal):gboolean;cdecl;external External_library name 'vte_terminal_get_mouse_autohide';

    { Reset the terminal, optionally clearing the tab stops and line history.  }
    procedure vte_terminal_reset(terminal:PVteTerminal; full:gboolean; clear_history:gboolean);cdecl;external External_library name 'vte_terminal_reset';

    procedure vte_terminal_get_cursor_position(terminal:PVteTerminal; column:Pglong; row:Pglong);cdecl;external External_library name 'vte_terminal_get_cursor_position';

    { Display string matching:  clear all matching expressions.  }
    procedure vte_terminal_match_clear_all(terminal:PVteTerminal);cdecl;external External_library name 'vte_terminal_match_clear_all';

    { Add a matching expression, returning the tag the widget assigns to that
     * expression.  }
(* Const before type ignored *)
    function vte_terminal_match_add(terminal:PVteTerminal; match:Pchar):longint;cdecl;external External_library name 'vte_terminal_match_add';

    { Set the cursor to be used when the pointer is over a given match.  }
    procedure vte_terminal_match_set_cursor(terminal:PVteTerminal; tag:longint; cursor:PGdkCursor);cdecl;external External_library name 'vte_terminal_match_set_cursor';

    procedure vte_terminal_match_set_cursor_type(terminal:PVteTerminal; tag:longint; cursor_type:TGdkCursorType);cdecl;external External_library name 'vte_terminal_match_set_cursor_type';

    { Remove a matching expression by tag.  }
    procedure vte_terminal_match_remove(terminal:PVteTerminal; tag:longint);cdecl;external External_library name 'vte_terminal_match_remove';

    { Check if a given cell on the screen contains part of a matched string.  If
     * it does, return the string, and store the match tag in the optional tag
     * argument.  }
    function vte_terminal_match_check(terminal:PVteTerminal; column:glong; row:glong; tag:Plongint):Pchar;cdecl;external External_library name 'vte_terminal_match_check';

    { Set the emulation type.  Most of the time you won't need this.  }
(* Const before type ignored *)
    procedure vte_terminal_set_emulation(terminal:PVteTerminal; emulation:Pchar);cdecl;external External_library name 'vte_terminal_set_emulation';

(* Const before type ignored *)
    function vte_terminal_get_emulation(terminal:PVteTerminal):Pchar;cdecl;external External_library name 'vte_terminal_get_emulation';

(* Const before type ignored *)
    function vte_terminal_get_default_emulation(terminal:PVteTerminal):Pchar;cdecl;external External_library name 'vte_terminal_get_default_emulation';

    { Set the character encoding.  Most of the time you won't need this.  }
(* Const before type ignored *)
    procedure vte_terminal_set_encoding(terminal:PVteTerminal; codeset:Pchar);cdecl;external External_library name 'vte_terminal_set_encoding';

(* Const before type ignored *)
    function vte_terminal_get_encoding(terminal:PVteTerminal):Pchar;cdecl;external External_library name 'vte_terminal_get_encoding';

    { Get the contents of the status line.  }
(* Const before type ignored *)
    function vte_terminal_get_status_line(terminal:PVteTerminal):Pchar;cdecl;external External_library name 'vte_terminal_get_status_line';

    { Get the padding the widget is using.  }
    procedure vte_terminal_get_padding(terminal:PVteTerminal; xpad:Plongint; ypad:Plongint);cdecl;external External_library name 'vte_terminal_get_padding';

    { Attach an existing PTY master side to the terminal widget.  Use
     * instead of vte_terminal_fork_command().  }
    procedure vte_terminal_set_pty(terminal:PVteTerminal; pty_master:longint);cdecl;external External_library name 'vte_terminal_set_pty';

    { Accessors for bindings.  }
    function vte_terminal_get_adjustment(terminal:PVteTerminal):PGtkAdjustment;cdecl;external External_library name 'vte_terminal_get_adjustment';

    function vte_terminal_get_char_width(terminal:PVteTerminal):glong;cdecl;external External_library name 'vte_terminal_get_char_width';

    function vte_terminal_get_char_height(terminal:PVteTerminal):glong;cdecl;external External_library name 'vte_terminal_get_char_height';

    function vte_terminal_get_char_descent(terminal:PVteTerminal):glong;cdecl;external External_library name 'vte_terminal_get_char_descent';

    function vte_terminal_get_char_ascent(terminal:PVteTerminal):glong;cdecl;external External_library name 'vte_terminal_get_char_ascent';

    function vte_terminal_get_row_count(terminal:PVteTerminal):glong;cdecl;external External_library name 'vte_terminal_get_row_count';

    function vte_terminal_get_column_count(terminal:PVteTerminal):glong;cdecl;external External_library name 'vte_terminal_get_column_count';

(* Const before type ignored *)
    function vte_terminal_get_window_title(terminal:PVteTerminal):Pchar;cdecl;external External_library name 'vte_terminal_get_window_title';

(* Const before type ignored *)
    function vte_terminal_get_icon_title(terminal:PVteTerminal):Pchar;cdecl;external External_library name 'vte_terminal_get_icon_title';

    {$IFDEF VTEVERSION_17}
    function vte_terminal_get_cursor_shape(terminal:PVteTerminal):TVteTerminalCursorShape;cdecl;external External_library name 'vte_terminal_get_cursor_shape';
    {$ENDIF}

    {$IFDEF VTEVERSION_20}
    function vte_terminal_get_child_exit_status(terminal:PVteTerminal):glong;cdecl;external External_library name 'vte_terminal_get_child_exit_status';

    function vte_terminal_get_pty (terminal:PVteTerminal):gint;cdecl;external External_library name 'vte_terminal_get_pty';
    {vte_terminal_get_pty has been deprecated since version 0.26 and should not be used in newly-written code.
    Use vte_terminal_get_pty_object() and vte_pty_get_fd()}

    procedure vte_terminal_set_cursor_shape(terminal:PVteTerminal; shape: TVteTerminalCursorShape);cdecl;external External_library name 'vte_terminal_set_cursor_shape';
    {$ENDIF}

    {$IFDEF VTEVERSION_24}

{    gboolean            vte_terminal_write_contents         (VteTerminal *terminal,
                                                         GOutputStream *stream,
                                                         VteTerminalWriteFlags flags,
                                                         GCancellable *cancellable,
                                                         GError **error);}
    {$ENDIF}
    {$IFDEF VTEVERSION_26}    //all untested unless marked otherwise
    function vte_terminal_fork_command_full(terminal:PVteTerminal;
                                            pty_flags: TVtePtyFlags;
                                            working_directory: Pchar;
                                            argv: PPchar;
                                            envv: PPchar;
                                            spawn_flags: TGSpawnFlags;
                                            child_setup: pointer; //pointer to GSpawnChildSetupFunc
                                            child_setup_data: gpointer;
                                            child_pid: gint;
                                            error: PGError): Boolean;cdecl;external External_library name 'vte_terminal_fork_command_full';

    function vte_terminal_get_pty_object(terminal:PVteTerminal): PVtePty; cdecl;external External_library name 'vte_terminal_get_pty_object';

    function vte_terminal_pty_new(terminal:PVteTerminal; flags: TVtePtyFlags; error: PGError): PVtePty; cdecl;external External_library name 'vte_terminal_pty_new';

    function vte_terminal_search_find_next(terminal:PVteTerminal): gboolean;cdecl;external External_library name 'vte_terminal_search_find_next';

    function vte_terminal_search_find_previous(terminal:PVteTerminal): gboolean;cdecl;external External_library name 'vte_terminal_search_find_previous';

    function vte_terminal_search_get_gregex(terminal:PVteTerminal): PGRegex;cdecl;external External_library name 'vte_terminal_search_get_gregex';

    function vte_terminal_search_get_wrap_around (terminal:PVteTerminal): gboolean;cdecl;external External_library name 'vte_terminal_search_get_wrap_around';

    procedure vte_terminal_search_set_gregex(terminal:PVteTerminal; regex: PGRegex); cdecl;external External_library name 'vte_terminal_search_set_gregex';

    procedure vte_terminal_search_set_wrap_around (terminal:PVteTerminal; wrap_around: gboolean); cdecl;external External_library name 'vte_terminal_search_set_wrap_around';

    procedure vte_terminal_watch_child(terminal:PVteTerminal; child_pid: gint); cdecl;external External_library name 'vte_terminal_watch_child';

    { Vte-Pty }
    function vte_pty_new (flags: TVtePtyFlags; error: PGError): PVtePty; cdecl;external External_library name 'vte_pty_new';

    function vte_pty_new_foreign(fd: gint; error: PGError): PVtePty; cdecl;external External_library name 'vte_pty_new_foreign';

    procedure vte_pty_close(pty: PVtePty); cdecl;external External_library name 'vte_pty_close';

    procedure vte_pty_child_setup(pty: PVtePty); cdecl;external External_library name 'vte_pty_child_setup';

    function vte_pty_get_fd(pty: PVtePty): gint; cdecl;external External_library name 'vte_pty_get_fd';

    function vte_pty_set_size(pty: PVtePty; rows: gint; columns: gint; error: PGError): gboolean; cdecl;external External_library name 'vte_pty_set_size';

    function vte_pty_get_size(pty: PVtePty; rows: Pgint; columns: Pgint; error: PGError): gboolean; cdecl;external External_library name 'vte_pty_get_size';

    procedure vte_pty_set_term(pty: PVtePty; emulation: PChar);cdecl;external External_library name 'vte_pty_set_term';

    function vte_pty_set_utf8(pty: PVtePty; utf8: gboolean; error: PGError): gboolean; cdecl;external External_library name 'vte_pty_set_utf8';
{$ENDIF}

implementation

end.
