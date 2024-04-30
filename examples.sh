curl -vsA GET -o /dev/null "http://127.0.0.1:3000/uname/check?ip_username=user1"
curl -X PATCH "http://127.0.0.1:3000/user/deactivate/username/user1"
curl -X GET "http://127.0.0.1:3000/post/getall?user1=1&user2=8"
