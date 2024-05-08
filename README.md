
# Eatsy!

## Overview

This project implements the backend API for Eatsy, a food-sharing platform. It is built in Haskell. It provides endpoints for user management, post creation, and post retrieval.

## Technologies Used

- **Language:** Haskell
- **Database:** Postgres (Hosted on Azure)
- **Database Abstraction:** Neurelo
- **Authentication:** WIP

## Setup

To run this project locally, follow these steps:

1. Clone this repository.
2. - Run `nix develop` to build out the dependencies.
   - If you do not have *nix*, use `cabal install` ( Highly recommend nix ).
3. Build the project  using `cabal build`.
4. Reproduce schema in Neurelo  using the **NSL** file in the neurelo directory.
5. Add *NEURELO_KEY* and *NEURELO_ENDPOINT* to env 
6. Start the server by running `cabal run`.
7. DB can be loaded with sample data by running scripts in the *load_db* dir
    Ex: `sh load_posts.sh`

## Endpoints

### User Endpoints

#### Check Username Availability

- **Endpoint:** `/check/uname`
- **Method:** `GET`
- **Description:** Checks if a username is available.
- **Parameters:**
  - `ip_username` (query parameter): The username to check.
- **Response:**
  - `200 OK` if the username is available.
  - `400 Bad Request` if no username is provided or the username is taken.

#### Check If Email Exists

- **Endpoint:** `/check/useremail`
- **Method:** `GET`
- **Description:** Checks if an email is already being used.
- **Parameters:**
  - `ip_user_email` (query parameter): The user email to check.
- **Response:**
  - `200 OK` if the exists or does not exist.
  - `400 Bad Request` if no exer email is provided.

#### Create User

- **Endpoint:** `/user/create`
- **Method:** `POST`
- **Description:** Creates a new user.
- **Body:** JSON object representing the user.
- **Response:**
  - `200 OK` if the user is created successfully.
  - `400 Bad Request` if there are validation errors or the user already exists.

#### Deactivate User

- **Endpoint:** `/user/deactivate/username`
- **Method:** `PATCH`
- **Description:** Deactivates an existing user.
- **Parameters:**
  - `username` (query parameter): The username whose account is to be deactivated.
  - **Response:**
  - `200 OK` if the user is deactivated successfully.
  - `400 Bad Request` if there are errors.
 
#### Connect User

- **Endpoint:** `/user/connect`
- **Method:** `POST`
- **Description:** Connects two users. A connection is unidirectional.
- **Body:** JSON object with the users to be connected.
- **Response:**
  - `200 OK` if the user connection is created successfully.
  - `400 Bad Request` if there are validation errors or the user already exists.

#### UserLogin

- **Endpoint:** `/login`
- **Method:** `POST`
- **Description:** Login for a user.
- **Body:** JSON object with the user email and password.
- **Response:**
  - `200 OK` if the credentials match those stored.
  - `400 Bad Request` if the email does not exist.
  - `401 Unauthorised` if the password does not match.


### Post Endpoints

#### Create Post


- **Endpoint:** `/post/connect`
- **Method:** `POST`
- **Description:** Creates a post.
- **Body:** JSON object with post data to be connected.
- **Response:**
  - `200 OK` if the user is created successfully.
  - `400 Bad Request` if there are validation errors or the user already exists.
 
#### Get All Posts of User


- **Endpoint:** `/post/getall`
- **Method:** `GET`
- **Description:** Gets all posts of mentioned user2 as user1 if a user connection is already created .
- **Parameters:**
  - `user1` (query parameter): The user_id of the user who is making the request.
 - `user2` (query parameter): The user_id of the user whose posts are to be displayed.
- **Response:**
  - `200 OK` if the user posts are successfully fetched.
  - `400 Bad Request` if there are validation errors or the user1 does not have a connection with user2.

##TODO:
1. Deactivate connection if user deactivates
3. Get a specific post
4. Delete a post
5. Add Likes to a post
6. Add categories to a post
7. add post types :
    - public (anyone can see the post)
    - connections (only connections can see the post)
    - shared_users_only (posts that can be shared to only specific users)
8. Posts Feed
