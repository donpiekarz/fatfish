# FatFish

FatFish is a special SMTP MTA server and Erlang/OTP doesn't make it special.


# Problem

User's mail box contains a lot of information and this information can be used by diffrent entities to e.g. reading user's mail for [Profiling](https://en.wikipedia.org/wiki/Profiling_%28information_science%29).

There are GPG and SMIME techniques, but lazy senders ignore it and send plain texts. Also neither GPG or SMIME cannot protect messege headers like: sender's address, date, subject, sender's mail application information and routing details. This information can be succesfuly mined.

# Solution

What FatFish does for incoming mail:

1. recives messages only for own users,
2. encrypts whole message (including headers) using user's certificate, e.g. RSA,
3. sends encrypted message to real user's address.

Yes, FatFish doesn't store any user's data except certificate and email address.
