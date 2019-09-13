(use-modules (web server)
             (web request)
             (web response)
             (web uri)
             (json)
             (srfi srfi-9)
             (ice-9 match))


;; Config

(define base-url
  (uri->string (build-uri 'http #:host "localhost" #:port 8080)))


;; Actors

(define-record-type <actor>
  (make-actor id name type inbox outbox)
  actor?
  (id actor-id)
  (name actor-name)
  (type actor-type)
  (inbox actor-inbox set-actor-inbox!)
  (outbox actor-outbox set-actor-outbox!))

(define (actor-outbox-uri actor)
  (string-append
   (actor-id actor)
   "/outbox"))

(define (actor-inbox-uri actor)
  (string-append
   (actor-id actor)
   "/inbox"))

(define (actor->scm actor)
  `(("id" . ,(actor-id actor))
    ("name" . ,(actor-name actor))
    ("type" . ,(actor-type actor))
    ("inbox" . ,(actor-inbox-uri actor))
    ("outbox" . ,(actor-outbox-uri actor))))

(define (actor-add-to-outbox! actor to-add)
  (set-actor-outbox! actor (cons to-add (actor-outbox actor))))


;; Object

(define-record-type <object>
  (make-object id type alist)
  object?
  (id object-id)
  (type object-type)
  (alist object-alist))

(define (object->scm object)
  (cons*
   `("id" . ,(object-id object))
   `("type" . ,(object-type object))
   (object-alist object)))


;; OrderedCollection

(define (as-ordered-collection stuff)
  `(("type" . "OrderedCollection")
    ("totalItems" . ,(length stuff))
    ("orderedItems" . ,(list->vector stuff))))



;; In memory key-value database

(define database '())

(define (db-add! id value)
  (set! database (acons id value database)))

(define (db-get id)
  (assoc-ref database id))

(define (db-add-actor! actor)
  (db-add! (actor-id actor) actor))

(define (db-dereference id)
  "Attempt to dereference objects by id from database.
  If object is not in db just return the id."
  (let ((dereferenced-thing (db-get id)))
    (if dereferenced-thing
        dereferenced-thing
        id)))

;; Add alice
(db-add-actor!
  (make-actor
   (string-append base-url "/actors/" "alice")
   "Alice"
   "Person"
   '()
   '()))


;; JSON helpers

(define* (respond-with-json obj #:key (code 200))
  (values (build-response #:code code
                          #:headers '((content-type . (application/json))))
          (lambda (port) (scm->json obj port))))


;; Web server

(define (not-found request)
  (values (build-response #:code 404
                          #:headers '((content-type . (text/plain))))
          (string-append "Resource not found: "
                         (uri->string (request-uri request)))))

(define (activitypub-actors-handler actor-path-components request request-body)
  "Handle requests for a specific actor"
  (let*
      (;; build the actor id from the request url
       (actor-id (string-append base-url "/actors/" (car actor-path-components)))
       ;; get actor from db
       (actor (db-get actor-id)))

    (if (actor? actor)

        (match `(,(request-method request) ,(cdr actor-path-components))

          ;; return the actor object
          ((GET ())
           (respond-with-json (actor->scm actor)))

          ((GET ("inbox"))
           (respond-with-json (as-ordered-collection
                               (map object->scm (actor-inbox actor)))))

          ((GET ("outbox"))
           (respond-with-json (as-ordered-collection
                               (map object->scm (actor-outbox actor)))))

          (_ (not-found request)))

        ;; if actor does not exist respond with 404
        (not-found request))))


(define (activitypub-handler request request-body)
  "ActivityPub requests handler"
  (let
      ((request-path-components
        (split-and-decode-uri-path (uri-path (request-uri request)))))

    ;; log the request
    (display (string-append
              (symbol->string (request-method request)) " "
              (uri->string (request-uri request)) "\n"))

    (match request-path-components

      (()
       (values '((content-type . (text/plain))) "index route"))

      (("test")
       (values '((content-type . (text/plain))) "Test"))

      (("actors" . actor-path-components)
       (activitypub-actors-handler actor-path-components request request-body))

      (_ (not-found request))

      )))


;; Run it!

(define (main)
  (display "Starting server at http://localhost:8080...\n")
  (run-server (lambda (request request-body)
                (activitypub-handler request request-body))
              'http '(#:port 8080)))

(main)
