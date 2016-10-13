# create 'src' directory
echo "Create 'src' directory..."
echo ""

mkdir src/

# tangle files
echo ""
echo "Start tangling files..."
echo ""

emacs -Q --batch --eval "(progn (require 'org)(require 'ob)(require 'ob-tangle) (progn (find-file (expand-file-name \"emacs.org\"))(org-babel-tangle)(kill-buffer)))"

# prepare .emacs
echo ""
echo "Write .emacs"
echo ""

echo "(load \"~/.emacs.d/init.el\")" > ~/.emacs

echo " ~> DONE!"
