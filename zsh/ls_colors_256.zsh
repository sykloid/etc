# ls_colors_256.zsh: Color definitions for the output of the ls command.
# P.C. Shyamshankar <sykora@lucentbeing.com>

typeset -TUg LS_COLORS ls_colors

ls_colors=(
    # Standard Descriptors.
    'no=00'                                 # Normal
    'fi=00'                                 # Files
    'di=(01);(38;05;63)'                    # Directories
    'ln=(04);(38;05;44)'                    # Links
    'pi=(38;05;88)'                         # Named Pipes
    'so=(38;05;252)'                        # Sockets
    'bd=(38;05;237)'                        # Block Devices
    'cd=(38;05;243)'                        # Character Devices
    'or=(01);(38;05;196)'                   # ???
    'mi=(01);(38;05;196)'                   # Missing Files
    'ex=(03);(38;05;46)'                    # Executables

    # Files, by extension.

    # Documents

    '*.pdf=(38;05;208)'

    # Images
    '*.bmp=(38;05;51)'
    '*.gif=(38;05;51)'
    '*.jpg=(38;05;51)'
    '*.png=(38;05;51)'
    '*.svg=(38;05;51)'
    '*.tif=(38;05;51)'
    '*.xbm=(38;05;51)'
    '*.xpm=(38;05;51)'

    # Audio
    '*.mp3=(38;05;141)'
    '*.ogg=(38;05;141)'
    '*.wav=(38;05;141)'
    '*.wma=(38;05;141)'

    # Video
    '*.avi=(38;05;63)'
    '*.divx=(38;05;63)'
    '*.mp4=(38;05;63)'
    '*.xvid=(38;05;63)'
)

export LS_COLORS
