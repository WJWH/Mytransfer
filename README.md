# Mytransfer
A clone of wetransfer.com in Haskell

## Project goals
* Use a cloud service as a storage backend to handle file storage (S3, GCS, etc).
Scaling is nice.
* Develop a nice WAI middleware to handle graceful shutdowns for autoscaling.
See the previous point.
* Make backgrounds scale to the device size of the user.
What is the point of having nice images as backgrounds if you can't use them?
* Log all the things
Haskell logging is a bit all over the place, what with it being impure and all. 
* Provide a lot of documentation and/or comments
It would be nice if people looking to do specific can use this project sa a reference. Thus, it should be as 

