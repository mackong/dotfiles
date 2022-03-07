set history filename ~/.gdb_history
set history save
set history size unlimited

# Pretty printing of structures
set print pretty

# Allow setting pending breakpoints
set breakpoint pending on

define td
    tui disable
end

define te
    tui enable
    # Display the source and assembly window.
    layout split
    focus cmd
end

define pwn
    source /usr/share/pwndbg/gdbinit.py
end
