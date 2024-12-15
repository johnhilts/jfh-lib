(in-package #:jfh-user-tests)

;; (defclass user-test-request (tbnl:request))
;; (defclass user-test-acceptor (tbnl:acceptor))

(defun test-user-auth ()
  (let ((jfh-store:*data-store-location* (make-instance 'jfh-store:data-store-location :user-path-root "../../../hokima/users/")))
    (jfh-testing:test-spec :category "User Auth"
      
      (jfh-testing:test-spec :description "With Login only (No password)"
        (jfh-testing:test-spec :it "Should find User Info given valid User Login."
          (jfh-user:get-secure-user-info "me@here.com"))
        (jfh-testing:test-spec :it "Should NOT find User Info given invalid User Login."
          (null (jfh-user:get-secure-user-info "fake@test.com"))))
      
      (jfh-testing:test-spec :description "With Login and Password."
        (jfh-testing:test-spec :it "Should Verify Password matches."
          (let ((user-info (jfh-user:get-secure-user-info "me@here.com"))
                (password "abc123"))
            (string= (jfh-user:user-password user-info) (jfh-user:hash-password password))))
        (jfh-testing:test-spec :it "Should Verify Password does NOT match."
          (let ((user-info (jfh-user:get-secure-user-info "me@here.com"))
                (password "xyz456"))
            (string/= (jfh-user:user-password user-info) (jfh-user:hash-password password)))))
      
      (jfh-testing:test-spec :description "With certificate fingerprint."
        (jfh-testing:test-spec :it "Should find User Info given valid fingerprint."
          (jfh-user:get-secure-user-info #(195 163 55 218 58 209 124 66 77 80 59 159 226 165 164 4 103 144 82 37 194 131 38 120 220 186 74 120 211 247 53 193)))
        (jfh-testing:test-spec :it "Should NOT find User Info given invalid fingerprint."
          (null (jfh-user:get-secure-user-info #())))))))
