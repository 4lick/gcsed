;;; gcsed-tests.el

(require 'dash)
(require 'gcsed-util)
(require 'gcsed-io)

(ert-deftest gcsed-gcs-ls-test ()
   (let* ((inhibit-message t)
          (res (gcsed-gcs-ls "gs://alick-dev/valid/")))
     (should (equal res '("gs://alick-dev/valid/test")))))

(ert-deftest gcsed-gcs-cp-test ()
   (let* ((inhibit-message t))
     ;;(gcsed-gcs-cp "gs://alick-dev/valid/" "gs://alick-dev/valid-copied/" t)
     (gcsed-gcs-cp "gs://alick-dev/valid/test" "gs://alick-dev/valid/test2")
     (should (equal (gcsed-gcs-ls "gs://alick-dev/valid/") '("gs://alick-dev/valid/test" "gs://alick-dev/valid/test2")))))

(ert-deftest gcsed-gcs-rm-test ()
   (let* ((inhibit-message t))
     (gcsed-gcs-rm "gs://alick-dev/valid/test2" t)
     (should (equal (gcsed-gcs-ls "gs://alick-dev/valid/") '("gs://alick-dev/valid/test")))))
