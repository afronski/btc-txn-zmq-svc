-record(latlng, { lat = 0.0, lng = 0.0 }).
-record(bounds, { nw = #latlng{}, se = #latlng{} }).
-record(state,  { timer, status, bounds = #bounds{} }).