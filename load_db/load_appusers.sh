#!/bin/bash

curl -X POST -d '{"username" : "user1", "user_email" : "user1@example.com", "user_pw" : "zzzz", "dob" : "1998-01-01" , "country" : "USA"}' "http://localhost:3000/user/create"
curl -X POST -d '{"username" : "user2", "user_email" : "user2@example.com", "user_pw" : "zzzz", "dob" : "1995-03-15" , "country" : "Canada"}' "http://localhost:3000/user/create"
curl -X POST -d '{"username" : "user3", "user_email" : "user3@example.com", "user_pw" : "zzzz", "dob" : "1990-07-20" , "country" : "UK"}' "http://localhost:3000/user/create"
curl -X POST -d '{"username" : "user4", "user_email" : "user4@example.com", "user_pw" : "zzzz", "dob" : "1985-09-10" , "country" : "Australia"}' "http://localhost:3000/user/create"
curl -X POST -d '{"username" : "user5", "user_email" : "user5@example.com", "user_pw" : "zzzz", "dob" : "1980-12-25" , "country" : "Japan"}' "http://localhost:3000/user/create"
curl -X POST -d '{"username" : "user6", "user_email" : "user6@example.com", "user_pw" : "zzzz", "dob" : "1975-04-30" , "country" : "Germany"}' "http://localhost:3000/user/create"
curl -X POST -d '{"username" : "user7", "user_email" : "user7@example.com", "user_pw" : "zzzz", "dob" : "1970-06-12" , "country" : "France"}' "http://localhost:3000/user/create"
curl -X POST -d '{"username" : "user8", "user_email" : "user8@example.com", "user_pw" : "zzzz", "dob" : "1965-11-05" , "country" : "Spain"}' "http://localhost:3000/user/create"
curl -X POST -d '{"username" : "user9", "user_email" : "user9@example.com", "user_pw" : "zzzz", "dob" : "1960-02-17" , "country" : "Italy"}' "http://localhost:3000/user/create"
curl -X POST -d '{"username" : "user10", "user_email" : "user10@example.com", "user_pw" : "zzzz", "dob" : "1955-08-22" , "country" : "Brazil"}' "http://localhost:3000/user/create"

