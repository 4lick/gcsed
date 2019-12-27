# gcsed _(WIP...)_

Emacs Tramp-like access to [Google Cloud Storage](https://cloud.google.com/storage/) _(strongly inspired by [s3ed](https://github.com/mattusifer/s3ed/))_.

### Opening files with `gcsed-find-file`

When you select a file with `gcsed-find-file`, the file will be downloaded from gcs to your local machine and opened. Saving that file will apply changes directly to gcs.

## Dependencies

- The `gsutil` cli should be on your `$PATH`

## Notes

- gcsed will use /tmp/gcsed/ as scratch space

## GCS Authentication

gcsed uses the `gsutil` cli to access to gcs, which has several options for authentication.
```
gcloud auth activate-service-account --key-file=credential.json
```
or
```
gsutil config -e
```
etc..
