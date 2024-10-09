# lucos_monitoring
Monitoring tool for lucos services

## Dependencies
* docker
* docker-compose

## Environment Variables

* SEND_ADDRESS - the email address notifications will come from
* SEND_PASSWORD - the password for the above email address
* SMTP_RELAY - the SMTP Relay used to log into the above email address
* TO_ADDRESS - The email address to send notifications to

## Running
`docker-compose up --no-build`

## Building
The build is configured to run on CircleCI when a commit is pushed to the `main` branch in github.

## Running Tests
`rebar3 do eunit --cover, cover`
