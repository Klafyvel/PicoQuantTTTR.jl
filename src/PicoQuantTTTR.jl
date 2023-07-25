module PicoQuantTTTR

using OrderedCollections
using Tables

abstract type TagType end
struct TypeEmpty8 <: TagType end
struct TypeBool8 <: TagType end
struct TypeInt8 <: TagType end
struct TypeBitSet64 <: TagType end
struct TypeColor8 <: TagType end
struct TypeFloat8 <: TagType end
struct TypeTDateTime <: TagType end
struct TypeFloat8Array <: TagType end
struct TypeAsciiString <: TagType end
struct TypeWideString <: TagType end
struct TypeBinaryBlob <: TagType end

struct Tag{T <: TagType}
    id::String
    idx::Int32
    value::Any
    enhancement::Any
end

function Base.show(io::IO, t::Tag)
    print(io, t.id)
    if t.idx != -1
        print(io, "[", t.idx, "]")
    end
    if !isnothing(t.enhancement)
        print(io, " : ", t.value, " ", t.enhancement)
    else
        print(io, " : ", t.value)
    end
end
function Base.show(io::IO, t::Tag{T}) where {T <: TypeEmpty8}
    print(io, t.id)
    if t.idx != -1
        print(io, "[", t.idx, "]")
    end
end
function Base.show(io::IO, t::Tag{T}) where {T <: Union{TypeAsciiString, TypeWideString}}
    print(io, t.id)
    if t.idx != -1
        print(io, "[", t.idx, "]")
    end
    print(io, " : \"", t.enhancement, "\"")
end

function dispatch_tag(f, tagtype::UInt32, args...; kwargs...)
    if tagtype == 0xFFFF0008
        f(TypeEmpty8(), args...; kwargs...)
    elseif tagtype == 0x00000008
        f(TypeBool8(), args...; kwargs...)
    elseif tagtype == 0x10000008
        f(TypeInt8(), args...; kwargs...)
    elseif tagtype == 0x11000008
        f(TypeBitSet64(), args...; kwargs...)
    elseif tagtype == 0x12000008
        f(TypeColor8(), args...; kwargs...)
    elseif tagtype == 0x20000008
        f(TypeFloat8(), args...; kwargs...)
    elseif tagtype == 0x21000008
        f(TypeTDateTime(), args...; kwargs...)
    elseif tagtype == 0x2001FFFF
        f(TypeFloat8Array(), args...; kwargs...)
    elseif tagtype == 0x4001FFFF
        f(TypeAsciiString(), args...; kwargs...)
    elseif tagtype == 0x4002FFFF
        f(TypeWideString(), args...; kwargs...)
    elseif tagtype == 0xFFFFFFFF
        f(TypeBinaryBlob(), args...; kwargs...)
    else
        error("Unknown tag type for dispatch_tag: $tagtype")
    end
end

function readtag(io)
    tagid = strip(String(read(io, 32)), '\0')
    tagidx = read(io, Int32)
    tagtypecode = read(io, UInt32)
    dispatch_tag(readtag, tagtypecode, io, tagid, tagidx)
end

function readtag(::TypeEmpty8, io, tagid, tagidx)
    Tag{TypeEmpty8}(tagid, tagidx, read(io, 8), nothing)
end
function readtag(::TypeBool8, io, tagid, tagidx)
    Tag{TypeBool8}(tagid, tagidx, any(read(io, 8) .!= 0), nothing)
end
function readtag(::TypeInt8, io, tagid, tagidx)
    Tag{TypeInt8}(tagid, tagidx, read(io, Int64), nothing)
end
function readtag(::TypeBitSet64, io, tagid, tagidx)
    Tag{TypeBitSet64}(tagid, tagidx, read(io, UInt64), nothing)
end
function readtag(::TypeColor8, io, tagid, tagidx)
    Tag{TypeColor8}(tagid, tagidx, read(io, UInt64), nothing)
end
function readtag(::TypeFloat8, io, tagid, tagidx)
    Tag{TypeFloat8}(tagid, tagidx, read(io, Float64), nothing)
end
function readtag(::TypeTDateTime, io, tagid, tagidx)
    Tag{TypeTDateTime}(tagid, tagidx, read(io, Float64), nothing)
end
function readtag(::TypeFloat8Array, io, tagid, tagidx)
    length = read(io, UInt32)
    vals = Vector{Float64}(undef, length / sizeof(Float64))
    readbytes!(io, vals)
    Tag{TypeFloat8Array}(tagid, tagidx, length, vals)
end
function readtag(::TypeAsciiString, io, tagid, tagidx)
    length = read(io, UInt64)
    vals = read(io, length)
    Tag{TypeAsciiString}(tagid, tagidx, length, strip(String(vals), '\0'))
end
function readtag(::TypeWideString, io, tagid, tagidx)
    length = read(io, UInt64)
    vals = Vector{UInt16}(undef, length / sizeof(UInt16))
    readbytes!(io, vals)
    Tag{TypeWideString}(tagid, tagidx, length, strip(transcode(String, vals), '\0'))
end
function readtag(::TypeBinaryBlob, io, tagid, tagidx)
    length = read(io, UInt64)
    vals = read(io, length)
    Tag{TypeBinaryBlob}(tagid, tagidx, length, vals)
end

struct Header
    magic::String
    version::String
    tags::OrderedDict{String, Tag}
end

function Base.show(io::IO, h::Header)
    print(io, "Header(", h.magic, ", ", h.version, ")\n\tTags:\n")
    for (_, t) in collect(h.tags)
        print(io, "\t\t", t, "\n")
    end
end

function readheader(io)
    magic = strip(String(read(io, 8)), '\0')
    version = strip(String(read(io, 8)), '\0')
    tags = OrderedDict{String, Tag}()
    tag = readtag(io)
    tags[tag.id] = tag
    while !eof(io) && tag.id != "Header_End"
        tag = readtag(io)
        tags[tag.id] = tag
    end
    Header(magic, version, tags)
end

Base.iterate(iter::Header) = iterate(iter.tags)
Base.iterate(iter::Header, state) = iterate(iter.tags, state)
Base.length(iter::Header) = length(iter.tags)
Base.getindex(h::Header, i) = getindex(h.tags, i).value

abstract type TCSPCRecordFormat end
struct PicoHarpT3 <: TCSPCRecordFormat end
struct PicoHarpT2 <: TCSPCRecordFormat end
struct HydraHarpV1T3 <: TCSPCRecordFormat end
struct HydraHarpV1T2 <: TCSPCRecordFormat end
struct HydraHarpV2T3 <: TCSPCRecordFormat end
struct HydraHarpV2T2 <: TCSPCRecordFormat end
struct TimeHarp260NT2 <: TCSPCRecordFormat end
struct TimeHarp260NT3 <: TCSPCRecordFormat end
struct TimeHarp260PT2 <: TCSPCRecordFormat end
struct TimeHarp260PT3 <: TCSPCRecordFormat end
struct MultiHarpT2 <: TCSPCRecordFormat end
struct MultiHarpT3 <: TCSPCRecordFormat end

function dispatch_recordformat(f, rec::Int64, args...; kwargs...)
    if rec == 0x00010303
        f(PicoHarpT3(), args...; kwargs...)
    elseif rec == 0x00010203
        f(PicoHarpT2(), args...; kwargs...)
    elseif rec == 0x00010304
        f(HydraHarpV1T3(), args...; kwargs...)
    elseif rec == 0x00010204
        f(HydraHarpV1T2(), args...; kwargs...)
    elseif rec == 0x01010304
        f(HydraHarpV2T3(), args...; kwargs...)
    elseif rec == 0x01010204
        f(HydraHarpV2T2(), args...; kwargs...)
    elseif rec == 0x00010305
        f(TimeHarp260NT3(), args...; kwargs...)
    elseif rec == 0x00010205
        f(TimeHarp260NT2(), args...; kwargs...)
    elseif rec == 0x00010306
        f(TimeHarp260NT3(), args...; kwargs...)
    elseif rec == 0x00010206
        f(TimeHarp260NT2(), args...; kwargs...)
    elseif rec == 0x00010307
        f(MultiHarpT3(), args...; kwargs...)
    elseif rec == 0x00010207
        f(MultiHarpT2(), args...; kwargs...)
    else
        error("Unknown TCSPC specific record type: $(string(rec, base=16))")
    end
end

overflowperiod(::PicoHarpT2) = 210698240
overflowperiod(::PicoHarpT3) = 65536
overflowperiod(::HydraHarpV1T2) = 33552000
function overflowperiod(::Union{HydraHarpV2T2, MultiHarpT2, TimeHarp260NT2, TimeHarp260PT2})
    33554432
end
function overflowperiod(::Union{HydraHarpV1T3, HydraHarpV2T3, MultiHarpT3, TimeHarp260NT3,
                                TimeHarp260PT3})
    1024
end

struct PicoT2Record
    channel::UInt8
    timetag::UInt32
    isoverflow::Bool
    isexternalmarker::Bool
end
isoverflow(r::PicoT2Record) = r.isoverflow
isevent(r::PicoT2Record) = !r.isexternalmarker && !isoverflow(r)
overflow(::PicoT2Record) = 1

struct PicoT3Record
    channel::UInt8
    dtime::UInt16
    nsync::UInt16
    isoverflow::Bool
    isexternalmarker::Bool
end
isoverflow(r::PicoT3Record) = r.isoverflow
isevent(r::PicoT3Record) = !r.isexternalmarker && !isoverflow(r)
overflow(::PicoT3Record) = 1

struct T2Record{T}
    isspecial::Bool
    channel::UInt8
    timetag::UInt32
end
isoverflow(r::T2Record) = r.isspecial && r.channel == 63
isevent(r::T2Record) = !r.isspecial
overflow(r::T2Record{T}) where {T} = r.timetag
overflow(r::T2Record{HydraHarpV1T2}) = 1

struct T3Record{T}
    isspecial::Bool
    channel::UInt8
    dtime::UInt16
    nsync::UInt16
end
isoverflow(r::T3Record) = r.isspecial && r.channel == 63
isevent(r::T3Record) = !r.isspecial
overflow(r::T3Record{T}) where {T} = r.nsync
overflow(r::T3Record{HydraHarpV1T2}) = 1

function readrecord(::PicoHarpT2, io)
    bytes = read(io, UInt32)
    channel = (bytes & 0xf0000000) >> 28
    timetag = bytes & 0x0fffffff
    isoverflow = channel == 0xf && (timetag & 0xf) == 0
    isexternalmarker = channel == 0xf && (timetag & 0xf) != 0
    PicoT2Record(channel, timetag, isoverflow, isexternalmarker)
end

function readrecord(::PicoHarpT3, io)
    bytes = read(io, UInt32)
    channel = (bytes & 0xf0000000) >> 28
    dtime = (bytes & 0x0fff0000) >> 16
    nsync = bytes & 0x0000ffff
    isoverflow = channel == 0xf && dtime == 0
    isexternalmarker = channel == 0xff && dtime != 0
    PicoT3Record(channel, dtime, nsync, isoverflow, isexternalmarker)
end

function readrecord(::T,
                    io) where {
                               T <: Union{HydraHarpV1T2, HydraHarpV2T2, MultiHarpT2,
                                     TimeHarp260NT2, TimeHarp260PT2}}
    bytes = read(io, UInt32)
    special = (bytes & 0x80000000) >> 31
    channel = (bytes & 0x7e000000) >> 25
    timetag = bytes & 0x01ffffff
    T2Record{T}(special, channel, timetag)
end

function readrecord(::T,
                    io) where {
                               T <: Union{HydraHarpV1T3, HydraHarpV2T3, MultiHarpT3,
                                     TimeHarp260NT3, TimeHarp260PT3}}
    bytes = read(io, UInt32)
    special = (bytes & 0x80000000) >> 31
    channel = (bytes & 0x7e000000) >> 25
    dtime = (bytes & 0x01fffc00) >> 10
    nsync = bytes & 0x3ff
    T3Record{T}(special, channel, dtime, nsync)
end

T2 = Union{PicoHarpT2, HydraHarpV1T2, HydraHarpV2T2, MultiHarpT2, TimeHarp260NT2, TimeHarp260PT2}
function readrecords(t::T2, header, io)
    number_of_overflows = 0
    time = 0.0
    channels = NamedTuple{(:channel, :time), Tuple{Int, Float64}}[]

    for _ in 1:header["TTResult_NumberOfRecords"]
        if eof(io)
            @warn "Your file is malformed, EOF reached before reading all records."
        end
        r = readrecord(t, io)
        if isoverflow(r)
            number_of_overflows += overflow(r)
        elseif isevent(r)
            time = (r.timetag + number_of_overflows * overflowperiod(t)) *
                   header["MeasDesc_GlobalResolution"]
            push!(channels, (channel = r.channel, time = time))
        else
            # We ignore the marker and sync events. Contact me if they are needed.
        end
    end
    channels
end

T3 = Union{PicoHarpT3, HydraHarpV1T3, HydraHarpV2T3, MultiHarpT3, TimeHarp260NT3, TimeHarp260PT3}
function readrecords(t::T3, header, io)
    number_of_overflows = 0
    time = 0.0
    dtime = 0.0
    channels = NamedTuple{(:channel, :time, :dtime), Tuple{Int, Float64, Float64}}[]

    for _ in 1:header["TTResult_NumberOfRecords"]
        if eof(io)
            @warn "Your file is malformed, EOF reached before reading all records."
        end
        r = readrecord(t, io)
        if isoverflow(r)
            number_of_overflows += overflow(r)
        elseif isevent(r)
            time = (r.nsync + number_of_overflows * overflowperiod(t)) *
                   header["MeasDesc_GlobalResolution"]
            dtime = r.dtime * header["MeasDesc_Resolution"]
            push!(channels, (channel = r.channel, time = time, dtime = dtime))
        else
            # We ignore the marker and sync events. Contact me if they are needed.
        end
    end
    channels
end

struct TTTR{T}
    header::Header
    records::Vector
end

function TTTR(t::T, header, io) where {T}
    records = readrecords(t, header, io)   
    TTTR{T}(header, records)
end
function TTTR(io)
    header = readheader(io)
    dispatch_recordformat(TTTR, header["TTResultFormat_TTTRRecType"], header, io)    
end
TTTR(s::AbstractString) = open(s) do io TTTR(io) end

function Base.show(io::IO, h::TTTR{T}) where {T}
    print(io, "TTTR{", T, "} with ", length(h.records), " records")
end

# Tables.jl interface
Tables.columnnames(::TTTR{T}) where {T <: T2} = [:channel, :time]
Tables.columnnames(::TTTR{T}) where {T <: T3} = [:channel, :time, :dtime]
Tables.istable(::Type{<:TTTR}) = true
Tables.schema(t::TTTR{T}) where {T <: T2} = Tables.Schema(Tables.columnnames(t), [Int, Float64])
Tables.schema(t::TTTR{T}) where {T <: T3} = Tables.Schema(Tables.columnnames(t), [Int, Float64, Float64])
Tables.columnaccess(::Type{<:TTTR}) = true
Tables.columns(t::TTTR) = t
Tables.getcolumn(t::TTTR, i) = Tables.getColumn(t, Tables.columnnames(t)[i])
function Tables.getcolumn(t::TTTR, nm::Symbol) 
    if nm ∉ Tables.columnnames(t)
        throw(ArgumentError("Column $nm does not exist for this TTR."))
    end
    map(Base.Fix2(getfield, nm), t.records)
end
function Base.getproperty(f::TTTR, sym::Symbol)
    try
        Tables.getcolumn(f, sym)
    catch e
        if e isa ArgumentError 
            getfield(f, sym)
        else
            rethrow(e)
        end
    end
end
function Base.propertynames(t::TTTR, private::Bool=false)
    [Tables.columnnames(t); fieldnames(TTTR)...]
end

export TTTR

end
