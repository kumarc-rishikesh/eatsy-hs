curl -X GET "http://127.0.0.1:3000/uname/check?ip_username=user1"
curl -X PATCH "http://127.0.0.1:3000/user/deactivate/user_id/5"
curl -X GET "http://127.0.0.1:3000/post/getall?user1=1&user2=8"
curl -X GET "http://localhost:3000/check/useremail?ip_user_email=user1@example.com"
curl -X POST -d '{"user_email" : "user1@example.com", "user_pw":"zzzz"}' "http://localhost:3000/login"
