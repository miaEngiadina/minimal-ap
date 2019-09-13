(use-modules (web server)
             (web request)
             (web response)
             (web uri)
             (srfi srfi-9)
             (ice-9 match))

;; Config

(define base-url "http://localhost:8080/")



;; Actors

(define-record-type <actor>
  (make-actor id type inbox outbox)
  actor?
  (id actor-id)
  (type actor-type)
  (inbox actor-inbox set-actor-inbox!)
  (outbox actor-outbox set-actor-outbox!))

(define (actor->json actor)
  `(@ ,@activitystreams-context
      (id ,(actor-id actor))
      (type ,(actor-type actor))
      (inbox ,(string-append (actor-id actor) "inbox"))
      (outbox ,(string-append (actor-id actor) "outbox"))
      ))

(define alice
  (make-actor
   (string-append base-url "actors/alice/")
   "Person"
   '()
   '()))

(define actors
  `(("alice" . ,alice)))



;; JSON helpers

(define activitystreams-context
  '(("@context" "https://www.w3.org/ns/activitystreams")))





;; Server

(define (not-found request)
  (values (build-response #:code 404
                          #:headers '((content-type . (text/plain))))
          (string-append "Resource not found: "
                         (uri->string (request-uri request)))))

(define (activitypub-actors-handler actor-name actor-path-components request request-body)
  "Handle requests for a specific actor"
  (display actor-name)
  (let ((actor (assoc-ref actors actor-name)))
    (display actor)
    (if (actor? actor)
        (values (build-response #:code 200 #:headers '((content-type . (application/json))))
                (lambda (port) (write-json (actor->json actor) port)))
        (not-found request))))

(define (activitypub-handler request request-body)
  "ActivityPub requests handler"
  (let
      (;; decode the request path
       (request-path-components
        (split-and-decode-uri-path (uri-path (request-uri request)))))

    (display (string-append (uri->string (request-uri request)) "\n"))

    (match request-path-components

      (()
       (values '((content-type . (text/plain))) "index route"))

      (("test")
       (values '((content-type . (text/plain))) "Test"))

      (("actors" actor-name . actor-path-components)
       (activitypub-actors-handler actor-name actor-path-components request request-body))

      (_
       (not-found request))

      )))



;; Run it!

(define (main)
  (display "Starting server at http://localhost:8080...\n")
  (run-server (lambda (request request-body)
                (activitypub-handler request request-body))
              'http '(#:port 8080)))

(main)
