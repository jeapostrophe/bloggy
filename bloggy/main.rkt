#lang racket/base
(require racket/unit
         racket/list
         racket/file
         racket/system
         racket/runtime-path
         racket/match
         racket/function
         racket/generator
         xml
         planet2/util)

(define-signature bloggy-config^
  (output
   *N*
   *BLOG-URL* *BLOG-TITLE* *BLOG-SUBTITLE* *BLOG-GA-ACCOUNT* *BLOG-DISQUS*
   *EXTRAS*))

(define-signature bloggy^
  (post! page! finalize!))

(define-runtime-path style.css-path
  "style.css")

(define (snoc l v)
  (append l (list v)))

(define (table-snoc! table top . more)
  (match more
    [(list value)
     (hash-update! table top
                   (λ (l)
                     (snoc l value))
                   empty)]
    [_
     (define next-table
       (hash-ref! table top make-hash))
     (apply table-snoc! next-table more)]))

(struct post-data (year month day filename title categories content-f))
(define (post-data-link pd)
  (match-define
   (post-data year month day filename title categories content-f) pd)
  `(a ([href ,(format "/posts/~a/~a/~a/~a" year month day filename)])
      ,title))

(define-unit bloggy@
  (import bloggy-config^) (export bloggy^)

  (when (directory-exists? output)
    (delete-directory/files output))
  (make-directory* output)

  (define year-table (make-hash))
  (define cat-table (make-hash))

  (define (template title title? body)
    `(html
      (head
       (meta ([charset "utf-8"]))
       (link ([rel "canonical"] [href ,*BLOG-URL*]))
       (link ([href "/style.css"]
              [rel "stylesheet"]
              [type "text/css"]))
       (link ([href "/atom.xml"]
              [rel "alternate"]
              [title ,*BLOG-TITLE*]
              [type "application/atom+xml"]))
       (title ,(format "~a > ~a" *BLOG-TITLE* title))
       (script ([type "text/javascript"])
               ,(format "var _gaq = _gaq || [];
    _gaq.push(['_setAccount', '~a']);
    _gaq.push(['_trackPageview']);

    (function() {
      var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
      ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
      var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);
    })();"
                        *BLOG-GA-ACCOUNT*)))
      (body
       (div ([class "header"])
            (span ([id "blog_title"]) (a ([href "/"]) ,*BLOG-TITLE*))
            (span ([id "blog_subtitle"]) ,*BLOG-SUBTITLE*))
       (div ([class "nav"])
            (ul
             (li (a ([href "/archive/"]) "Archive"))
             (li (a ([href "/cat/"]) "Categories"))
             ,@(for/list ([a*t (in-list *EXTRAS*)])
                 `(li (a ([href ,(car a*t)]) ,(cdr a*t))))
             (li ([class "rss"]) (a ([href "/atom.xml"]) "RSS"))))
       (div ([class "content"])
            ,(if title? `(span ([id "content_title"]) ,title) "")
            ,body))))

  (define (post-data-long pd #:comments? [comments? #t])
    (match-define
     (post-data year month day filename title categories content-f) pd)
    (define filename-name
      ;; XXX
      filename)
    `(div ([class "post"])
          (span ([class "title"])
                (a ([href
                     ,(format "/posts/~a/~a/~a/~a"
                              year month day filename)])
                   ,title))
          (span ([class "date"])
                (span ([class "year"])
                      (a ([href ,(format "/archive/~a/" year)])
                         ,year))
                (span ([class "month"])
                      (a ([href ,(format "/archive/~a/~a/" year month)])
                         ,month))
                (span ([class "day"])
                      (a ([href ,(format "/archive/~a/~a/~a/"
                                         year month day)])
                         ,day)))
          (pre ([class "content"])
               ,@(add-between (content-f) "\n"))
          (ul ([class "categories"])
              ,@(for/list ([cat (in-list categories)])
                  `(li (a ([href ,(format "/cat/~a/" cat)]) ,cat))))
          ,(if comments?
             `(div ([id "disqus_thread"] [aria-live "polite"])
                   (noscript "Please enable JavaScript to view the "
                             (a ([href "http://disqus.com/?ref_noscript"])
                                "comments powered by Disqus."))
                   (script ([type "text/javascript"])
                           ,(format "var disqus_shortname = '~a';

        // var disqus_developer = 1;
        var disqus_identifier = 'http://~a/blog/~a/~a/~a/~a/';
        var disqus_url = 'http://~a/posts/~a/~a/~a/~a';
        var disqus_script = 'embed.js';

    (function () {
      var dsq = document.createElement('script'); dsq.type = 'text/javascript'; dsq.async = true;
      dsq.src = 'http://' + disqus_shortname + '.disqus.com/' + disqus_script;
      (document.getElementsByTagName('head')[0] || document.getElementsByTagName('body')[0]).appendChild(dsq);
    }());"
                                    *BLOG-DISQUS*
                                    *BLOG-URL*
                                    year
                                    month
                                    day
                                    filename-name
                                    *BLOG-URL*
                                    year
                                    month
                                    day
                                    filename)))
             "")))
  (define (post-data-short pd)
    (post-data-long pd #:comments? #f))

  (define (output-raw! path xexpr-f)
    (define the-path
      (build-path output
                  path))
    (make-parent-directory* the-path)
    (with-output-to-file the-path
      #:exists 'replace
      (λ ()
        (write-xexpr (xexpr-f)))))

  (define (output! path title title? xexpr-f)
    (output-raw! path (λ () (template title title? (xexpr-f)))))

  (define (post! #:filename filename
                 #:title title
                 #:date date
                 #:categories categories
                 content-f)
    (match-define (vector year month day) date)
    (define path (build-path "posts" year month day filename))
    (define this-post-data
      (post-data year month day filename title categories content-f))
    (table-snoc! year-table year month day this-post-data)
    (for ([cat (in-list categories)])
      (table-snoc! cat-table cat this-post-data))
    (output! path title #f (λ () (post-data-long this-post-data))))

  (define (page! #:path path
                 #:title title
                 #:title? [title? #t]
                 content-f)
    (output! path title title? content-f))

  (define (years-page!)
    (page! #:path (build-path "archive" "index.html")
           #:title "Archive"
           (λ ()
             `(ul
               ,@(for/list ([year (in-sorted-hash-keys year-table)])
                   `(li (a ([href ,(format "/archive/~a/" year)])
                           ,year)))))))

  (define (year-page! year month-table)
    (page! #:path (build-path "archive" year "index.html")
           #:title (format "~a" year)
           (λ ()
             `(ul
               ,@(for/list ([month (in-sorted-hash-keys month-table)])
                   `(li (a ([href ,(format "/archive/~a/~a/" year month)])
                           ,month)))))))

  (define (month-page! year month day-table)
    (page! #:path (build-path "archive" year month "index.html")
           #:title (format "~a > ~a" year month)
           (λ ()
             `(ul
               ,@(for/list ([day (in-sorted-hash-keys day-table)])
                   `(li (a ([href ,(format "/archive/~a/~a/~a/" year month day)])
                           ,day)))))))

  (define (day-page! year month day day-posts)
    (page! #:path (build-path "archive" year month day "index.html")
           #:title (format "~a > ~a > ~a" year month day)
           (λ ()
             `(ul
               ,@(for/list ([post (in-list day-posts)])
                   `(li ,(post-data-link post)))))))

  (define (categories-page!)
    (page! #:path (build-path "cat" "index.html")
           #:title "Categories"
           (λ ()
             `(ul
               ,@(reverse
                  (for/list ([cat (in-sorted-hash-keys cat-table)])
                    `(li (a ([href ,(format "/cat/~a/" cat)])
                            ,cat))))))))

  (define (cat-page! cat cat-posts)
    (page! #:path (build-path "cat" cat "index.html")
           #:title (format "~a" cat)
           (λ ()
             `(ul
               ,@(for/list ([post (in-list cat-posts)])
                   `(li ,(post-data-link post)))))))

  (define (in-sorted-hash-keys ht)
    (in-list (sort (hash-keys ht) string>?)))

  (define (in-all-posts)
    (in-generator
     (for ([year (in-sorted-hash-keys year-table)])
       (define month-table (hash-ref year-table year))
       (for ([month (in-sorted-hash-keys month-table)])
         (define day-table (hash-ref month-table month))
         (for ([day (in-sorted-hash-keys day-table)])
           (define day-posts (hash-ref day-table day))
           (for ([p (in-list day-posts)])
             (yield p)))))))

  (define (index-page!)
    (page! #:path (build-path "index.html")
           #:title "Home"
           #:title? #f
           (λ ()
             `(div ([class "posts"])
                   ,@(for/list ([post (in-all-posts)]
                                [i (in-range *N*)])
                       (post-data-short post))))))

  (define (atom!)
    (output-raw!
     (build-path "atom.xml")
     (λ ()
       (match-define
        (post-data last-year last-month last-day _ _ _ _)
        (for/or ([pd (in-all-posts)]
                 [i (in-range 1)])
          pd))
       `(feed ([xmlns "http://www.w3.org/2005/Atom"])
              (title ,(cdata #f #f (format "<![CDATA[~a]]>" *BLOG-TITLE*)))
              (link ([href ,(format "~a/atom.xml" *BLOG-URL*)]
                     [rel "self"]))
              (link ([href ,*BLOG-URL*]))
              (updated ,(format "~a-~a-~aT00:00:00-00:00"
                                last-year last-month last-day))
              (id ,*BLOG-URL*)
              ,@(for/list ([pd (in-all-posts)]
                           [i (in-range *N*)])
                  (match-define
                   (post-data year month day filename
                              title categories content-f) pd)
                  (define this-url
                    (format "http://~a/post/~a/~a/~a/~a"
                            *BLOG-URL*
                            year month day filename))
                  `(entry
                    (title ([type "html"])
                           ,(cdata #f #f (format "<![CDATA[~a]]>" title)))
                    (link ([href
                            ,this-url]))
                    (updated ,(format "~a-~a-~aT00:00:00-00:00"
                                      year month day))
                    (id ,this-url)
                    (content ([type "html"])
                             ,(cdata #f #f
                                     (format "<![CDATA[~a]]>"
                                             (xexpr->string
                                              (post-data-short pd)))))))))))

  ;; XXX test google analytics
  ;; XXX test comment links

  (define (finalize!)
    (years-page!)
    (for ([(year month-table) (in-hash year-table)])
      (year-page! year month-table)
      (for ([(month day-table) (in-hash month-table)])
        (month-page! year month day-table)
        (for ([(day day-posts) (in-hash day-table)])
          (day-page! year month day day-posts))))

    (categories-page!)
    (for ([(cat cat-posts) (in-hash cat-table)])
      (cat-page! cat cat-posts))

    (index-page!)
    (atom!)

    (copy-file style.css-path (build-path output "style.css"))))

(provide (all-defined-out))
