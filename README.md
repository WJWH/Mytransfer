# Mytransfer
A clone of wetransfer.com in Haskell, just to see how it works and if I can do it.

## Project goals
* Use a cloud service as a storage backend to handle file storage (S3, GCS, etc). Scaling is nice.
* Allow for "proper" autoscaling. Users should never notice a server being spun down, so it should at least have nice connection draining. The load balancers of AWS and GCE only allow connection draining for 3600 seconds though, which may not be enough for users downloading really large files on really slow connections, so it should be implemented on the servers itself.
* Make backgrounds scale to the device size of the user. What is the point of having nice images as backgrounds if the user can't see them well?
* Log all the things. Haskell logging is a bit all over the place, what with it being impure and all. Status: unstarted, though it uses the standard WAI middleware for logging requests at least.

## NOT project goals
* Actually run a cloud storage service. It sounds like a lot of hassle. Don't rely on the website actually being up.
* Make a super nice frontend using all the hippest technologies. In this project I will be focusing on the backend and only adding as much frontend as is required to make the application work.

## Personal goals
* Write a medium complexity webapp using Scotty.
* Figure out the Google Cloud Storage APIs.

