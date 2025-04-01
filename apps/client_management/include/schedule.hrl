-record(schedule, {
    id,
    user_id,
    title,
    datetime,
    repeat = none,         % none | daily | weekly
    reminder_at = 15       % minutes before event
}).
