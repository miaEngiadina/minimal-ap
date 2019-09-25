;; Copyright © 2019 pukkamustard <pukkamustard@posteo.net>
;;
;; This file is part of minimal-ap.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(use-modules (web server)
             (web request)
             (web response)
             (web uri)
             (json)
             (srfi srfi-1)
             (srfi srfi-9)
             (srfi srfi-19)
             (ice-9 match)
             (ice-9 receive)
             (rnrs bytevectors))



;; Config

(define base-url
  (uri->string (build-uri 'http #:host "localhost" #:port 8080)))


;; Actors

(define-record-type <actor>
  (make-actor id name type)
  actor?
  (id actor-id)
  (name actor-name)
  (type actor-type))

(define (actor-outbox actor)
  "returns id of the actors outbox"
  (string-append
   (actor-id actor)
   "/outbox"))

(define (actor-inbox actor)
  "returns id of the actors inbox"
  (string-append
   (actor-id actor)
   "/inbox"))

(define (actor-liked actor)
  "returns id of the actors liked collection"
  (string-append
   (actor-id actor)
   "/liked"))

(define (actor->scm actor)
  `(("id" . ,(actor-id actor))
    ("name" . ,(actor-name actor))
    ("type" . ,(actor-type actor))
    ("inbox" . ,(actor-inbox actor))
    ("outbox" . ,(actor-outbox actor))
    ("liked" . ,(actor-liked actor))))


;; Activities

;; ActivityStreams Activity
(define-record-type <activity>
  (make-activity id type actor published properties object)
  activity?
  (id activity-id)
  (type activity-type)
  (actor activity-actor)
  (published activity-published)
  (properties activity-properties)
  (object activity-object))

(define (activity->scm activity)
  "serialize an actor record"
  (append
   `(("id" . ,(activity-id activity))
     ("type" . ,(activity-type activity))
     ("published" . ,(date->string (activity-published activity) "~4"))
     ("actor" . ,(if (actor? (activity-actor activity))
                    (actor-id (activity-actor activity))
                    (activity-actor activity)))
     ("object" . ,(dereference-and-serialize (activity-object activity))))
   (activity-properties activity)))

(define (activity-type? type)
  "Returns true if type is a valid ActivityStreams activity type."
  (member type ;; TODO support all Activity types
          '("Create" "Like")))

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


(define (object-id object)
  (cond
   ;; object is just a string, hopefully an id
   ((string? object) object)

   (else (assoc-ref object "id"))))


;; Collection

(define-record-type <collection>
  (make-collection id items)
  collection?
  (id collection-id)
  (items collection-items set-collection-items!))

(define (collection->scm collection)
  (let ((items (collection-items collection)))
    `(("id" . ,(collection-id collection))
      ("type" . "Collection")
      ("totalItems" . ,(length items))
      ("items" . ,(list->vector (map dereference-and-serialize items))))))


;; In memory key-value database

;; alist of actors, activities and objects with ids as keys
(define database '())

(define (reset-database!)

  (set! database '())

  ;; Add initial actors
  (map add-actor!
       ;; Alice
       `(,(make-actor
           (string-append base-url "/actors/" "alice")
           "Alice"
           "Person")

         ;; and Bob
         ,(make-actor
           (string-append base-url "/actors/" "bob")
           "Bob"
           "Person")))

  ;; Add the special public collection
  (add-object! "https://www.w3.org/ns/activitystreams#Public"
               (make-collection "https://www.w3.org/ns/activitystreams#Public" '())))

(define (add-object! id value)
  "Add object to database"
  (set! database (acons id value database)))

(define (get-object id)
  "Get an object from the databse"
  (assoc-ref database id))

(define (add-actor! actor)
  "Add an actor to the database"

  ;; create a new inbox and outbox collection
  (let ((inbox (make-collection (actor-inbox actor) '()))
        (outbox (make-collection (actor-outbox actor) '()))
        (liked (make-collection (actor-liked actor) '())))

    ;; add actor to database
    (add-object! (actor-id actor) actor)

    ;; add inbox, outbox and liked collection to database
    (add-object! (collection-id inbox) inbox)
    (add-object! (collection-id outbox) outbox)
    (add-object! (collection-id liked) liked)))

(define (add-to-collection! collection item)
  (cond
   ((collection? collection)
    ;; add to collection with set-collection-items! mutator
    (set-collection-items! collection (cons item (collection-items collection))))

   ((not collection)
    ;; if collection is #f don't do anything. This prevents infinite cycles caused by condition below.
    #f)

   ((get-object collection)
    ;; if collection is not a collection, attempt to dereference and retry
    (add-to-collection! (get-object collection) item))

   (else #f)))

(define (dereference-and-serialize id)
  "Dereference an object from database and serialize"
  (let ((dereferenced-thing (get-object id)))
    (match dereferenced-thing

      (($ <actor> ) (actor->scm dereferenced-thing))

      (($ <activity>) (activity->scm dereferenced-thing))

      (($ <collection>) (collection->scm dereferenced-thing))

      ;; if object could not be dereferenced just return the id
      (#f id)

      ;; if it is something else return the derefenced thing
      (_ dereferenced-thing)
      )))


;; ID generator

(define (make-id-generator)
  (let ((id-counter 0))
    (lambda ()
      (set! id-counter (+ id-counter 1))
      id-counter)))

(define generate-id! (make-id-generator))

(define (generate-object-id!)
  (string-append base-url "/objects/" (number->string (generate-id!))))


;; JSON helpers

(define* (respond-with-json obj #:key (code 200))
  (values (build-response #:code code
                          #:headers '((content-type . (application/json))
                                      (Access-Control-Allow-Origin . "*")
                                      (Access-Control-Allow-Headers . "Authorization, Content-type")
                                      (Access-Control-Allow-Methods . "GET, POST")
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


(define (submission->activity-object actor submission)
  "Create an activity and object from submission"
  ;; TODO this only makes sense with Create activities, what about other things...
  (if
   (activity-type? (assoc-ref submission "type"))

   (let ((object
          (if (string? (assoc-ref submission "object"))

              ;; object in submission is (hopefully) an id
              (assoc-ref submission "object")

              ;; else create an object with an id
              (cons
               ;; with the newly generated id
               `("id" . ,(generate-object-id!))
               ;; but without any existing id
               (filter
                (lambda (pair) (not (member (car pair) '("id"))))
                (assoc-ref submission "object"))))))

     (values

      ;; Return a freshly created activity
      (make-activity
       (generate-object-id!)
       (assoc-ref submission "type")
       (actor-id actor)
       (current-date)
       ;; all other properties
       (filter (lambda (pair)
                 (not
                  (member (car pair) '("id" "type" "actor" "object"))))
               submission)
       (object-id object))

      ;; Return the object
      object))

   ;; else wrap in a Create activity record and retry
   ;; TODO: copy recipients from object to activity
   (submission->activity-object actor `(("type" . "Create")
                                        ("object" . ,submission)))))

(define (handle-submission-to-outbox actor request request-body)
  "Handle a POST to an actor's outbox"
  (receive (activity object)
      (submission->activity-object actor
                                   ;; parse submission from Json
                                   (json-string->scm (utf8->string request-body))
                                   )


    ;; add activity to database
    (add-object! (activity-id activity) activity)

    ;; add object to database, if not a reference (a string)
    (unless (string? object)
      (add-object! (object-id object) object))

    ;; add reference to activity in the actor outbox
    (add-to-collection! (actor-outbox actor) (activity-id activity))

    ;; deliver activity to recipients
    (map
     (lambda (recipient-id)

       (display
        (string-append "Delivering activity " (activity-id activity) " to " recipient-id "\n"))

       (let
           ;; attempt to retrieve actor from database
           ((recipient (get-object recipient-id)))

         (match recipient
           (($ <actor>)
            ;; if actor could be retrieved add activity to inbox of actor
            (add-to-collection! (actor-inbox recipient) (activity-id activity)))

           (($ <collection>)
            ;; if recipient is collection add activity
            (add-to-collection! recipient (activity-id activity)))

           (_
            ;; Don't know what to do...
            (display
             (string-append "Don't know how to deliver to " recipient-id "\n")))

           )))

     (activity-recipients activity))

    ;; If it is a Like activity add object to the actors liked collection
    (when (string= (activity-type activity) "Like")
      (add-to-collection! (actor-liked actor) (activity-object activity)))

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

          ;; respond with the actor's inbox
          (('GET ("inbox"))
           (respond-with-json (dereference-and-serialize (actor-inbox actor))))

          ;; respond with the actor's outbox
          (('GET ("outbox"))
           (respond-with-json (dereference-and-serialize (actor-outbox actor))))

          ;; handle a submission to the actor's inbox
          (('POST ("outbox"))
           (handle-submission-to-outbox actor request request-body))

          ;; respond with actors liked collection
          (('GET ("liked"))
           (respond-with-json (dereference-and-serialize (actor-liked actor))))

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

      (("api" "dev" "reset")
       (begin
         (reset-database!)
         (respond-with-json '(("msg" . "ok")))))

      (("actors" . actor-path-components)
       (activitypub-actors-handler actor-path-components request request-body))


      (("public")
       ;; the special public collection
       (respond-with-json (dereference-and-serialize "https://www.w3.org/ns/activitystreams#Public")))

      (("objects" . object-path-components)
       (let ((object
              ;; attempt to retrieve object from database
              (dereference-and-serialize
               (string-append base-url "/objects/" (car object-path-components)))))
         (if object
             (respond-with-json object)
             ;; if object is not in db respond with 404
             (not-found request))))

      (_ (not-found request))

      )))


;; Run it!

(define (main)
  (display "Starting server at http://localhost:8080...\n")
  (run-server (lambda (request request-body)
                (activitypub-handler request request-body))
              'http '(#:port 8080)))

(reset-database!)

(main)
