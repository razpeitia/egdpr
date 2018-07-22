-module(range_entry).
-author("Ricardo Azpeitia Pimentel").

-record(range_entry, {
	start_id,
	end_id
}).

-export([
	new/1,
	new/2,
	enforce_max_vendor_id/2
]).


new(Id) ->
	#range_entry{start_id = Id, end_id = Id}.

new(StartId, EndId) ->
	#range_entry{start_id = StartId, end_id = EndId}.

enforce_max_vendor_id(#range_entry{start_id = StartId, end_id = EndId}, MaxVendorId) ->
	case StartId > MaxVendorId orelse EndId > MaxVendorId of
		true ->
			Message = "Start VendorId must not be greater than End VendorId and "
                      "End VendorId must not be greater than Max Vendor Id",
			throw({parse_error, Message});
		false -> ok
	end.