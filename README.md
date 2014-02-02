Shoe-database-API
=================

A web API for interaction with a shoe database.

**Installation requirements**

*Database*
This particular implementation uses SQLite3 for database interaction.  For testing, an empty database named "shoe.sqlite3" should be created with the following table schema: CREATE TABLE shoes ("id" INTEGER PRIMARY KEY AUTOINCREMENT, "description" text,"size" int, "color" text);

*Directory structure*
A directory named "/images" should be created in the server root directory. This will be the storage location for all user-submitted images.

*Haskell requirements*
This module makes use of GHC's "DeriveGeneric" and "OverloadedStrings" language pragama and thus requires the use of the GHC compiler.  My own tests were conducted with GHC version 7.6.3.

Required Haskell libraries:
- aeson (>= 0.6.2.1)
- direct-sqlite (>= 2.3.9)
- http-types (>= 0.8.3)
- scotty (>= 0.6.2)

**USE**

The API is as follows, as per the job application requirements:
- Server accepts POST requests via "/json". Upon successful submission, the server returns a JSON object of the form {ok: "ok", redirect: "/shoe/<id>"}, where <id> is the database ID of the newly submitted data. A JPEG image file submitted with this request (as a base-64-encoded string) is stored in /images/shoe-<id>.jpg.
- GET requests via "/shoe/<id>" where <id> is a number, returns an HTML response containing the submitted description, size, color and image file.  Images are stored as files on the server, and <img> tags pointing to these images are returned with the HTML response.  For example "/shoe/4" returns data on the shoe with ID #4 in the database, as well as an image tag pointing to /images/shoe-4.jpg.
- GET request via "/" or "/index" returns an HTML response containing links to all of the entries in the database.

Addtionally, raw images may be accessed via "/images/<id>.jpg".

A sample client-side interface has also been provided in the form of the "json-form.html" file which submits user-supplied data to the server as an encoded JSON object.  The server will respond to "/json-form" with the contents of this file.
