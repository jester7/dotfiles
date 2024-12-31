;; (setq-default abbrev-mode t)

(setq save-abbrevs 'silently)

(add-hook 'mhtml-mode-hook #'abbrev-mode)
(add-hook 'css-mode-hook #'abbrev-mode)

(define-skeleton css-media-query
  "Responsive media query"
  "Breakpoint (px): "
  "@media screen and (max-width: " str "px) {\n"
  "    " _ "\n"
  "}")


(define-abbrev-table 'css-mode-abbrev-table
  '(("flex" "display: flex;\n    justify-content: center;\n    align-items: center;")
    ("grid" "display: grid;\n    grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));")
    ("pos" "position: absolute;\n    top: 0;\n    left: 0;")
    ("ani" "@keyframes name {\n    0% { }\n    100% { }\n}")
    ("mq" "" css-media-query)
    ("html" "" (lambda () (skeleton-insert '(nil "html { " _ "; }"))))
    ("box" "box-sizing: border-box;\n    padding: 0;\n    margin: 0;")
    ("boxshad" "box-shadow: 1px 1px 4px rgba(0,0,0,0.5);")
    ("trans" "transition: all 0.3s ease;")))

;; (define-abbrev-table 'html-mode-abbrev-table
;;   '(("<a" "<a href=\"\">" _ "</a>")
;;     ("<p" "<p>" _ "</p>")
;;     ("<h1" "<h1>" _ "</h1>")
;;     ("<h2" "<h2>" _ "</h2>")
;;     ("<h3" "<h3>" _ "</h3>")
;;     ("<div" "<div>" _ "</div>")
;;     ("<span" "<span>" _ "</span>")
;;     ("<img" "<img src=\"\" alt=\"\"" _ "/>")
;;     ("<ul" "<ul>\n    <li>" _ "</li>\n</ul>")
;;     ("<ol" "<ol>\n    <li>" _ "</li>\n</ol>")
;;     ("<li" "<li>" _ "</li>")
;;     ("<form" "<form action=\"\" method=\"\">\n    " _ "\n</form>")
;;     ("<input" "<input type=\"\" name=\"\"" _ ">")
;;     ("<label" "<label for=\"\">" _ "</label>")))

(define-abbrev-table 'mhtml-mode-abbrev-table
  '(("<a>" "" (lambda () (skeleton-insert '(nil "<a href=\"\">" _ "</a>"))))
    ("<p>" "" (lambda () (skeleton-insert '(nil "<p>" _ "</p>"))))
    ("<h1>" "" (lambda () (skeleton-insert '(nil "<h1>" _ "</h1>"))))
    ("<h2>" "" (lambda () (skeleton-insert '(nil "<h2>" _ "</h2>"))))
    ("<img>" "" (lambda () (skeleton-insert '(nil "<img src=\"\" alt=\"\"" _ "/>"))))
    ("<div>" "" (lambda () (skeleton-insert '(nil "<div>" _ "</div>"))))))
