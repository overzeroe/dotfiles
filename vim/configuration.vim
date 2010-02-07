" Main Vim configuration file "

" ****************************************************************************** "
" Note: 
"   This is my personal .vimrc file; copying the entire thing is a bad idea,
"   as it might produce weird side effects. To make it more readable, the top
"   section is restricted to easy to understand one-line configurations; 
"   anything that takes more than that is separated into a function or separate
"   source file. 
" ****************************************************************************** "


function! VimSetup()
" ************************   Quick Config   ************************************ "

" Line numbers "
set number

" Tab = Four Spaces "
call TabBehaviour()

" Traditional search "
call SearchBehaviour()

" Do not use arrow keys for movement "
call DisableArrowKeys()

" Hide annoying files "
call BackupAndSwapFiles()

" Syntax and highlighting "
call SyntaxHighlighting()

" Enable code folding "
call CodeFolding()

" Enable autoindent "
call AutoIndent()

" Ignore these files with these extensions when auto-completing files "
set wildignore=*.o,*.obj,*.exe,*.jpg,*.gif,*.png

" Use shell-like autocompletion "
set wildmode=longest:list

" Mappings "

" Press space to enter ex command mode "
map <Space> :

" Steal a few from Emacs "

" Use Ctrl-e and Ctrl-a to jump to end/start of lines, like Emacs "
imap <C-e> <ESC>$a
imap <C-a> <ESC>^i
map  <C-e>  <ESC>$
map  <C-a> <ESC>^

" Kill line "
map <C-k> D
imap <C-k> <ESC>Da

" Always keep a few lines above/below the cursor for context "
set scrolloff=5

" Use Conque plugin for shell "
command! Sh Conque zsh


" **********************   End Quick Config   ********************************** "
endfunction



" **************************   Functions   ************************************* "

" Tabs: Expand tabs to four spaces each. "
function! TabBehaviour()
    " Use spaces, not real tabs "
    set expandtab

    " When indenting with cindent, >>, <<, etc, use 4 spaces "
    set shiftwidth=4

    " When hitting tab or backspace, a tab should be 4 spaces "
    set softtabstop=4

    " For consistency, treat even real tabs as 4 spaces "
    set tabstop=4

    " If cursor is at 3 spaces, you press >>, go to 4, not 7 "
    set shiftround
endfunction


" Search: incremental search that isn't stupid "
function! SearchBehaviour()
    " Incremental search "
    set incsearch

    " Ignore case of search strings, unless capitals are included "
    set ignorecase
    set smartcase
endfunction


" Arrow Keys: Disable arrow keys for movement, use hjkl instead"
function! DisableArrowKeys()
    " Normal mode " 
    map <Down> <Nop>
    map <Up> <Nop>
    map <Right> <Nop>
    map <Left> <Nop>

    " Insert mode "
    imap <Down> <Nop> 
    imap <Up> <Nop>
    imap <Right> <Nop>
    imap <Left> <Nop>
endfunction


" Backup And Swap Files: Keep in ~/.vim/tmp/backup and ~/.vim/tmp, respectively "
function! BackupAndSwapFiles()
    " Make backup files in .vim/tmp/backup "
    set backup
    set backupdir=~/.vim/tmp/backup

    " Put swap files (.swo, .swp) in .vim/tmp "
    set directory=~/.vim/tmp
endfunction

" Syntax Highlighting: enabled, color-themed, and customized "
function! SyntaxHighlighting()
    " Enable syntax highlighting "
    syntax on

    " Use a colorscheme so that not everything has to be hand-set "
    " Source: vimcolorschemetest.googlecode.com/svn/colors/af.vim
    source ~/.vim/colorscheme.vim
endfunction


" Code Folding: allow code folding for functions, etc "
function! CodeFolding()
    " Enable code folding "
    set foldenable

    " C-style folding "
    set foldmethod=marker
    set foldmarker={,}

    " Don't autofold more than 3 levels "
    set foldminlines=5
endfunction

" Autoindent: enable autoindentation for c and other languages "
function! AutoIndent()
    set smartindent
endfunction

" ************************   End Functions   ************************************ "

" Run the configuration "
call VimSetup()
