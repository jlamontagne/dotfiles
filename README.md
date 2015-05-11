# Dot Files

## Profiling slow vim plugins

From http://stackoverflow.com/questions/12213597/how-to-see-which-plugins-are-making-vim-slow

http://stackoverflow.com/a/12216578

You can use built-in profiling support: after launching vim do

    :profile start profile.log
    :profile func *
    :profile file *
    " At this point do slow actions
    :profile pause
    :noautocmd qall!

(unlike quitting noautocmd is not really required, it just makes vim quit faster).

Note: you won’t get information about functions there were deleted before vim quit.
