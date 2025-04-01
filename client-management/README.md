client-management
=====

An OTP library

Build
-----

    $ rebar3 compile


How to Run the Scheduling & Reminders Service
=============================================

This service helps manage court hearings, appointments, and deadlines using Erlang's `gen_server`.

Step-by-step Instructions
-------------------------

1. Compile the project:

   rebar3 compile

2. Start the Erlang shell:

   rebar3 shell

3. Load the schedule record:

   In the Erlang shell, type:

   rr("apps/client_management/include/schedule.hrl").

Example Usage
-------------

Start the server:

   schedule_server:start_link().

Add a schedule:

   Now = calendar:local_time(),
   schedule_server:add_schedule(#schedule{
       id = 1,
       user_id = 123,
       title = "Court Meeting",
       datetime = Now,
       reminder_at = 1
   }),
   timer:sleep(100).

Get reminders:

   schedule_server:get_reminders(123).

Delete a schedule:

   schedule_server:delete_schedule(1),
   timer:sleep(100),
   schedule_server:get_reminders(123).  % should return []

Notes
-----
