(use-modules (web server)
             (web request)
             (web response)
             (web uri)
             (json)
             (srfi srfi-9)
             (srfi srfi-1)
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

(define (actor-add-to-outbox! actor object-id)
  "Add a reference to an object in the actors outbox"
  (set-actor-outbox! actor (cons object-id (actor-outbox actor))))

(define (actor-add-to-inbox! actor object-id)
  "Add a reference to an object in the actors inbox"
  (set-actor-inbox! actor (cons object-id (actor-inbox actor))))


;; Activities

;; ActivityStreams Activity
(define-record-type <activity>
  (make-activity id type actor properties object)
  activity?
  (id activity-id)
  (type activity-type)
  (actor activity-actor)
  (properties activity-properties)
  (object activity-object))

(define (activity->scm activity)
  "serialize an actor record"
  (append
   `(("id" . ,(activity-id activity))
     ("type" . ,(activity-type activity))
     ("actor" . ,(if (actor? (activity-actor activity))
                    (actor-id (activity-actor activity))
                    (activity-actor activity)))
     ("object" . ,(activity-object activity)))
   (activity-properties activity)))

(define (activity-type? type)
  "Returns true if type is a valid ActivityStreams activity type."
  (if (member type
              ;; TODO add all Activity types
              '("Create" "Accept" "Add" "Announce" "Arrive" "Block"))
      #t
      #f))

(define (activity-recipients activity)
  "Returns a list of recipients of the activity"
  (fold (lambda (recipient-field recipients)
          (let
              ;; get the value of the recipient field
              ((recipient-field-value
                (assoc-ref (activity-properties activity) recipient-field)))
            (cond

             ;; if vector add all elements to recipients
             ((vector? recipient-field-value)
              (append (vector->list recipient-field-value) recipients))

             ;; if string add to recipients
             ((string? recipient-field-value)
              (cons recipient-field-value recipients))

             ;; else ignore the field value
             (else recipients))))
        ;; initialize list of recipients
        '()
        ;; fields to search for recipients
        '("to" "bto" "cc" "bcc" "audience")))

(define (deliver-activity activity)
  "Deliver activity to recipients."
  (map
   (lambda (recipient)
     (display
      (string-append "Delivering activity " (activity-id activity) " to " recipient "\n"))
     (let
         ;; attempt to retrieve actor from database
         ((actor (get-object recipient)))

       (if (actor? actor)

           ;; if actor could be retrieved add activity to inbox of actor
           (actor-add-to-inbox! actor (activity-id activity))

           ;; else return false
           #f)))
   (activity-recipients activity)))

(define (alist->activity id actor alist)
  "Cast an alist to an activity or wrap in a Create activity if it not already an activity."
  (if
   ;; object is already an ActivityStreams activity
   (activity-type? (assoc-ref alist "type"))

   ;; then cast it into an activity record
   (make-activity
    id
    (assoc-ref alist "type")
    actor
    ;; store all other properties
    (filter (lambda (pair)
              (not
               (member (car pair) '("id" "type" "actor" "object"))))
            alist)
    (assoc-ref alist "object"))

   ;; else wrap in a Create activity record
   (make-activity id "Create" actor '() alist)))


;; OrderedCollection

(define (as-ordered-collection stuff)
  `(("type" . "OrderedCollection")
    ("totalItems" . ,(length stuff))
    ("orderedItems" . ,(list->vector stuff))))



;; In memory key-value database

;; alist of actors, activities and objects with ids as keys
(define database '())

(define (add-object! id value)
  "Add object to database"
  (set! database (acons id value database)))

(define (get-object id)
  "Get an object from the databse"
  (assoc-ref database id))

(define (add-actor! actor)
  "Add an actor to the database"
  (add-object! (actor-id actor) actor))

(define (dereference-and-serialize id)
  "Dereference an object from database and serialize"
  (let ((dereferenced-thing (get-object id)))
    (match dereferenced-thing

      (($ <actor> ) (actor->scm dereferenced-thing))

      (($ <activity>) (activity->scm dereferenced-thing))

      ;; if object could not be dereferenced just return the id
      (#f id)

      ;; if it is something else return the derefenced thing
      (_ dereferenced-thing)
      )))

;; counter for generated ids
(define id-counter 1)

(define (create-id!)
  (set! id-counter (+ id-counter 1))
  id-counter)

(define (create-object-id!)
  (string-append base-url "/objects/" (number->string (create-id!))))


;; JSON helpers

(define* (respond-with-json obj #:key (code 200))
  (values (build-response #:code code
                          #:headers '((content-type . (application/json))
                                      (Access-Control-Allow-Origin . "*")
                                      ))
          (lambda (port) (scm->json
                          ;; make it look like JSON-LD
                          (cons '("@context" . "https://www.w3.org/ns/activitystreams")
                                obj)
                          port))))


;; Web server

(define (not-found request)
  (values (build-response #:code 404
                          #:headers '((content-type . (text/plain))))
          (string-append "Resource not found: "
                         (uri->string (request-uri request)))))


(define (handle-submission-to-outbox actor request request-body)
  "Handle a POST to an actor's outbox"
  (let* (;; parse submission from JSON
         (submission (json-string->scm (utf8->string request-body)))

         ;; generate id for activity
         (generated-activity-id (create-object-id!))

         ;; TODO generate a separate id for the object

         ;; cast/wrap in an activity
         (activity (alist->activity generated-activity-id actor submission)))

    ;; add activity to database
    (add-object! (activity-id activity) activity)

    ;; add reference to activity in the actor outbox
    (actor-add-to-outbox! actor (activity-id activity))

    ;; deliver activity to recipients
    (deliver-activity activity)

    ;; respond with the created Activity
    (respond-with-json (activity->scm activity))))


(define (activitypub-actors-handler actor-path-components request request-body)
  "Handle requests for a specific actor"
  (let*
      (;; build the actor id from the request url
       (actor-id (string-append base-url "/actors/" (car actor-path-components)))
       ;; get actor from db
       (actor (get-object actor-id)))

    (if (actor? actor)

        (match `(,(request-method request) ,(cdr actor-path-components))

          ;; respond with the actor object
          (('GET ())
           (respond-with-json (actor->scm actor)))

          ;; respond with the actor's inbox as an OrderedCollection
          (('GET ("inbox"))
           (respond-with-json (as-ordered-collection
                               (map dereference-and-serialize (actor-inbox actor)))))

          ;; respond with the actor's outbox as an OrderedCollection
          (('GET ("outbox"))
           (respond-with-json (as-ordered-collection
                               (map dereference-and-serialize (actor-outbox actor)))))

          ;; handle a submission to the actor's inbox
          (('POST ("outbox"))
           (handle-submission-to-outbox actor request request-body))

          (('OPTIONS _)
           (values (build-response #:code 204
                                   #:headers '((Access-Control-Allow-Origin . "*")
                                               (Access-Control-Allow-Headers . "Authorization, Content-type")
                                               (Access-Control-Allow-Methods . "GET, POST"))) ""))

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

      (("api" "ap" "whoami")
       (respond-with-json (actor->scm (get-object "http://localhost:8080/actors/alice"))))

      (("actors" . actor-path-components)
       (activitypub-actors-handler actor-path-components request request-body))

      ;; TODO add route for accessing objects
      (("objects" . object-path-components)
       (respond-with-json "TODO"))

      (_ (not-found request))

      )))


;; Run it!

(define (main)
  (display "Starting server at http://localhost:8080...\n")
  (run-server (lambda (request request-body)
                (activitypub-handler request request-body))
              'http '(#:port 8080)))

;; Add initial actors
(map add-actor!
     ;; Alice
     `(,(make-actor
        (string-append base-url "/actors/" "alice")
        "Alice"
        "Person"
        '()
        '())

       ;; and Bob
       ,(make-actor
        (string-append base-url "/actors/" "bob")
        "Bob"
        "Person"
        '()
        '())))

(main)
