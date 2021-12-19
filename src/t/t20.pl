plan(+!isolate, true, !ensure_room_clean; !ensure_pantry_stocked; !lock_door).
plan(+!ensure_room_clean, true, !clean_off_cart;(write('Room Cleaned!'),nl)).
plan(+!ensure_pantry_stocked, true, (write('Pantry Stocked!'),nl)).
plan(+!lock_door, true, (write('Locked Door!'),nl)).
plan(+!clean_off_cart, true, (write('Cart Cleaned!'),nl)).