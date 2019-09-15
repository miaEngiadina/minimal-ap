(use-modules (web server)
             (web request)
             (web response)
             (web uri)
             (json)
             (srfi srfi-9)
             (ice-9 match)
             (rnrs bytevectors))


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


;; Activities

(define (activity-type? type)
  "Returns true if type is a valid ActivityStreams activity type."
  (if (member type
              ;; TODO add all Activity types
              '("Create" "Accept" "Add" "Announce" "Arrive" "Block"))
      #t
      #f))

(define (activity? object)
  (activity-type? (assoc-ref object "type")))

(define (wrap-in-create-activity object)
  "Wrap object in a Create activity if it not already an activity."
  (if (activity? object)
      object
      `(("type" . "Create")
        ("object" . ,object))))


;; OrderedCollection

(define (as-ordered-collection stuff)
  `(("type" . "OrderedCollection")
    ("totalItems" . ,(length stuff))
    ("orderedItems" . ,(list->vector stuff))))



;; In memory key-value database

;;; Alist of actors with ids as keys
(define actors-database '())

(define (add-actor! actor)
  "Add an actor to the database"
  (set! actors-database (acons (actor-id actor) actor actors-database)))

(define (get-actor id)
  "Get actor from database"
  (assoc-ref actors-database id))

;; Everything else that is not an actor gets dumped in here
(define objects-database '())

(define (add-object! id value)
  "Add object to database"
  (set! objects-database (acons id value objects-database)))

(define (get-object id)
  "Get an object from the databse"
  (assoc-ref objects-database id))

(define (dereference-object id)
  "Dereference an object if it exists in the database. Unlike get-object this returns the id if there is no associated object in database."
  (let ((dereferenced-thing (get-object id)))
    (if dereferenced-thing
        dereferenced-thing
        id)))

;; Add alice
(add-actor!
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

(define (handle-submission-to-outbox actor request request-body)
  "Handle a POST to an actor's outbox"
  (let* (;; Parse submission from JSON
         (submission (json-string->scm (utf8->string request-body)))
         (activity (assoc-set!
                    ;; wrap as Create activity
                    (wrap-in-create-activity submission)
                    ;; attribute activity to the actor
                    "attributedTo" (actor-id actor))))
    (actor-add-to-outbox! actor activity)
    (respond-with-json activity)))

(define (activitypub-actors-handler actor-path-components request request-body)
  "Handle requests for a specific actor"
  (let*
      (;; build the actor id from the request url
       (actor-id (string-append base-url "/actors/" (car actor-path-components)))
       ;; get actor from db
       (actor (get-actor actor-id)))

    (if (actor? actor)

        (match `(,(request-method request) ,(cdr actor-path-components))

          ;; respond with the actor object
          (('GET ())
           (respond-with-json (actor->scm actor)))

          ;; respond with the actor's inbox as an OrderedCollection
          (('GET ("inbox"))
           (respond-with-json (as-ordered-collection
                               (map dereference-object (actor-inbox actor)))))

          ;; respond with the actor's outbox as an OrderedCollection
          (('GET ("outbox"))
           (respond-with-json (as-ordered-collection
                               (map dereference-object (actor-outbox actor)))))

          ;; handle a submission to the actor's inbox
          (('POST ("outbox"))
           (handle-submission-to-outbox actor request request-body))

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
