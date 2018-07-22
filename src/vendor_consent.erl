-module(vendor_consent).
-author("Ricardo Azpeitia Pimentel").

-record(vendor_consent, {
	version,
	created,
	last_update,
	cpm_id,
	cpm_version,
	consent_screen,
	consent_language,
	vendor_list_version,
	purposes_allowed,
	max_vendor_id,
	encoding_type,
	bit_field,
	default_consent,
	num_entries,
	range_entries,
	integer_purposes
}).

-export([
	new/12,
	new/14,
	parse_consent_string/1
]).

-define(VENDOR_ENCODING_RANGE, 1).
-define(VERSION_BIT_SIZE, 6).
-define(CREATED_BIT_SIZE, 36).
-define(UPDATED_BIT_SIZE, 36).
-define(CMP_ID_SIZE, 12).
-define(CMP_VERSION_SIZE, 12).
-define(CONSENT_SCREEN_SIZE, 6).
-define(CONSENT_LANGUAGE_SIZE, 12).
-define(VENDOR_LIST_VERSION_SIZE, 12).
-define(PURPOSE_SIZE, 1).
-define(PURPOSES_SIZE, 24).
-define(MAX_VENDOR_ID_SIZE, 16).
-define(ENCODING_TYPE_SIZE, 1).
-define(DEFAULT_CONSENT_SIZE, 1).
-define(NUM_ENTRIES_SIZE, 12).
-define(VENDOR_ID_SIZE, 16).
-define(SINGLE_VENDOR, 0).
-define(VENDOR_TYPE_SIZE, 1).
-define(RANGE_VENDOR, 1).
-define(ENABLE, 1).
-define(DISABLE, 0).

new(Version, Created, LastUpdate, CpmId, CpmVersion,
	ConsentScreen, ConsentLanguage, VendorListVersion,
	PurposesAllowed, MaxVendorId, EncodingType,
	DefaultConsent, NumEntries, RangeEntries) ->
	#vendor_consent{
		version = Version,
		created = Created,
		last_update = LastUpdate,
		cpm_id = CpmId,
		cpm_version = CpmVersion,
		consent_screen = ConsentScreen,
		consent_language = ConsentLanguage,
		vendor_list_version = VendorListVersion,
		purposes_allowed = PurposesAllowed,
		max_vendor_id = MaxVendorId,
		encoding_type = EncodingType,
		default_consent = DefaultConsent,
		num_entries = NumEntries,
		range_entries = RangeEntries
	}.

new(Version, Created, LastUpdate, CpmId, CpmVersion,
	ConsentScreen, ConsentLanguage, VendorListVersion,
	PurposesAllowed, MaxVendorId, EncodingType,
	BitField) ->
	#vendor_consent{
		version = Version,
		created = Created,
		last_update = LastUpdate,
		cpm_id = CpmId,
		cpm_version = CpmVersion,
		consent_screen = ConsentScreen,
		consent_language = ConsentLanguage,
		vendor_list_version = VendorListVersion,
		purposes_allowed = PurposesAllowed,
		max_vendor_id = MaxVendorId,
		encoding_type = EncodingType,
		bit_field = BitField
	}.

parse_consent_string(ConsentString) ->
	RawConsentString = decode_consent_string(ConsentString),
	<<
		Version:?VERSION_BIT_SIZE,
		Created:?CREATED_BIT_SIZE,
		LastUpdate:?UPDATED_BIT_SIZE,
		CpmId:?CMP_ID_SIZE,
		CpmVersion:?CMP_VERSION_SIZE,
		ConsentScreen:?CONSENT_SCREEN_SIZE,
		ConsentLanguage:?CONSENT_LANGUAGE_SIZE,
		VendorListVersion:?VENDOR_LIST_VERSION_SIZE,
		BitsPurposesAllowed:?PURPOSES_SIZE,
		MaxVendorId:?MAX_VENDOR_ID_SIZE,
		EncodingType:?ENCODING_TYPE_SIZE,
		Rest/bitstring
	>> = RawConsentString,
	
	PurposesAllowed = parse_allowed_purposes(BitsPurposesAllowed),
	<<DefaultConsent:?DEFAULT_CONSENT_SIZE, 
	  NumEntries:?NUM_ENTRIES_SIZE,
	  RestEntries/bitstring>> = Rest,
	case EncodingType of
		?VENDOR_ENCODING_RANGE ->
			RangeEntries = parse_range_entries(RestEntries, MaxVendorId),
			enforce_num_entries(RangeEntries, NumEntries),
			new(Version, Created, LastUpdate, CpmId, CpmVersion,
				ConsentScreen, ConsentLanguage, VendorListVersion,
				PurposesAllowed, MaxVendorId, EncodingType,
				DefaultConsent, NumEntries, RangeEntries);
		_ ->
			enforce_bit_field(RestEntries, MaxVendorId),
			new(Version, Created, LastUpdate, CpmId, CpmVersion,
				ConsentScreen, ConsentLanguage, VendorListVersion,
				PurposesAllowed, MaxVendorId, EncodingType,
				RestEntries)
	end.

parse_allowed_purposes(Bitstring) ->
	parse_allowed_purposes(Bitstring, 1, []).

parse_allowed_purposes(<<>>, _Counter, Acc) ->
	lists:reverse(Acc);
parse_allowed_purposes(<<?ENABLE:?PURPOSE_SIZE, Rest/bitstring>>, Counter, Acc) ->
	parse_allowed_purposes(Rest, Counter + 1, [Counter | Acc]);
parse_allowed_purposes(<<?DISABLE:?PURPOSE_SIZE, Rest/bitstring>>, Counter, Acc) ->
	parse_allowed_purposes(Rest, Counter + 1, Acc).

parse_range_entries(RawEntries, MaxVendorId) ->
	parse_range_entries(RawEntries, MaxVendorId, []).

parse_range_entries(<<>>, _MaxVendorId, Acc) -> lists:reverse(Acc);
parse_range_entries(<<?SINGLE_VENDOR:?VENDOR_TYPE_SIZE, Id:?VENDOR_ID_SIZE, Rest/bitstring>>, MaxVendorId, Acc) ->
	RangeEntry = range_entry:new(Id),
	range_entry:enforce_max_vendor_id(RangeEntry, MaxVendorId),
	parse_range_entries(Rest, MaxVendorId, [RangeEntry | Acc]);
parse_range_entries(<<?RANGE_VENDOR:?VENDOR_TYPE_SIZE, StartId:?VENDOR_ID_SIZE, EndId:?VENDOR_ID_SIZE, Rest/bitstring>>, MaxVendorId, Acc) ->
	RangeEntry = range_entry:new(StartId, EndId),
	range_entry:enforce_max_vendor_id(RangeEntry, MaxVendorId),
	parse_range_entries(Rest, MaxVendorId, [RangeEntry | Acc]).

enforce_num_entries(RangeEntries, NumEntries) ->
	case length(RangeEntries) == NumEntries of
		false ->
			Message = "Number of entries doesn't match parsed entries",
			throw({parse_error, Message});
		true -> ok
	end.

enforce_bit_field(RestEntries, MaxVendorId) ->
	case bit_size(RestEntries) > MaxVendorId of
		true -> 
			Message = "VendorId provided in the bit field "
					  "must not be greater than Max VendorId",
			throw({parse_error, Message});
		false -> ok
	end.

decode_consent_string(ConsentString) ->
	List = tolist(ConsentString),
	tobin(base64_decode(List)).

base64_decode(String) ->
	base64:decode(String).

tobin(String) when is_binary(String) -> String;
tobin(String) when is_list(String) -> tobin(list_to_binary(String)).

tolist(Binary) when is_list(Binary) -> Binary;
tolist(Binary) when is_binary(Binary) -> binary_to_list(Binary).