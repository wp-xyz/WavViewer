unit wvWav;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  // http://soundfile.sapp.org/doc/WaveFormat/

  TWavRiffHeader = packed record
    ID: array[0..3] of ansichar;        // "RIFF" for little-endian, "RIFX" for big-endian
    ChunkSize: LongInt;                 // Filesize - 8
    RiffType: array[0..3] of ansiChar;  // "WAVE"
  end;

  TWavFmtSubchunk = packed record
    ID: array[0..3] of ansichar;  // "fmt "
    SubChunkSize: LongInt;   // 16
    AudioFormat: SmallInt;   // PCM = 1 --> uncompressed
    NumChannels: SmallInt;   // Mono = 1, Stereo = 2
    SampleRate: LongInt;     // 8000, 44100 etc.
    ByteRate: LongInt;       // SampleRate * NumChannels * BitsPerSample / 8
    BlockAlign: SmallInt;    // NumChannels * BitsPerSample / 8
    BitsPerSample: SmallInt; // 8, 16
  end;

  TWavDataSubchunk = packed record
    ID: array[0..3] of ansichar;  // "data"
    DataSize: LongInt;            // NumSamples * NumChannels * BitsPerSample / 8
                                  // or: size of following data part.
  end;

  // Data follow: first all data of right channel, followed by all data of left channel
  // BUT: de.wikipedia.org/wiki/RIFF_WAVE says that left channel is stored first!

procedure WriteWavHeader(AStream: TStream;
  ANumChannels, ASampleRate, ABitsPerSample: Integer);

procedure WriteWavToStream_Mono(AStream: TStream;
  ASampleRate, ABitsPerSample: Integer;
  ABuffer: Pointer; ABufferSize: LongInt);

procedure WriteWavToStream_Stereo(AStream: TStream;
  ASampleRate, ABitsPerSample: Integer; ADataLayout: String;
  ABuffer: Pointer; ABufferSize: LongInt);


implementation

procedure WriteWavHeader(AStream: TStream;
  ANumChannels, ASampleRate, ABitsPerSample: Integer);
var
  riff: TWavRiffHeader;
  fmt: TWavFmtSubchunk;
  data: TWavDataSubchunk;
begin
  riff.ID := 'RIFF';
  riff.ChunkSize := 0;   // Write later!!!!
  riff.RiffType := 'WAVE';
  AStream.WriteBuffer(riff, Sizeof(riff));

  fmt.ID := 'fmt ';
  fmt.SubchunkSize := 16;
  fmt.AudioFormat := 1;  // 1 = PCM = uncompressed
  fmt.NumChannels := ANumChannels;
  fmt.SampleRate := ASampleRate;
  fmt.BitsPerSample := ABitsPerSample;
  fmt.ByteRate := ANumchannels * ASampleRate * fmt.BitsPerSample div 8;
  fmt.BlockAlign := ANumChannels * fmt.BitsPerSample div 8;
  AStream.WriteBuffer(fmt, SizeOf(fmt));

  data.ID := 'data';
  AStream.WriteBuffer(data, SizeOf(data));
  // Later: write the size of data data part following here to the field
  // data.DataSize, i.e. 4 bytes below current stream position.
end;

procedure WriteWavToStream_Mono(AStream: TStream; ASampleRate, ABitsPerSample: Integer;
  ABuffer: Pointer; ABufferSize: LongInt);
var
  p: Int64;
begin
  WriteWavHeader(AStream, 1, ASampleRate, ABitsPerSample);
  p := AStream.Position;  // Indicates begin of data section

  // Write data
  AStream.WriteBuffer(ABuffer^, ABufferSize);

  // Complete missing header data
  AStream.Position := p - 4;
  AStream.WriteDWord(ABufferSize);  // Size of data part
  AStream.Position := 4;
  ASTream.WriteDWord(AStream.Size - 8);  // Size of RIFF chunk
end;

procedure WriteWavToStream_Stereo(AStream: TStream;
  ASampleRate, ABitsPerSample: Integer; ADataLayout: String;
  ABuffer: Pointer; ABufferSize: LongInt);
var
  p: Int64;
  ptr: Pointer;
  bytesPerSample: Integer;
  pb: PByte;
  pi: PSmallInt;
  i: Integer;
begin
  ADataLayout := Uppercase(ADataLayout);

  // Write wav header
  WriteWavHeader(AStream, 2, ASampleRate, ABitsPerSample);
  p := AStream.Position;  // Remember begin of data section

  // Data are interleaved i.e. L-R-L-R-...L-R or R-L-R-L-...-R-L
  if (ADataLayout = 'LRLR') or (ADataLayout = 'RLRL')  then
  begin
    bytesPerSample := ABitsPerSample div 8;
    ptr := ABuffer;
    if ADataLayout = 'RLRL' then inc(ptr, bytesPerSample);
    i := 0;
    while (i < ABufferSize) do
    begin
      AStream.WriteBuffer(ptr^, bytesPerSample);
      inc(i, 2);
      inc(ptr, bytesPerSample*2);
    end;
    ptr := ABuffer;
    if ADataLayout = 'LRLR' then inc(ptr, bytesPerSample);
    i := 0;
    while (i < ABufferSize) do
    begin
      AStream.WriteBuffer(ptr^, bytesPerSample);
      inc(i, 2);
      inc(ptr, bytesPerSample*2);
    end;
  end
  else
  if (ADatalayout = 'LLRR') or (ADatalayout = 'RRLL') then
  begin
    // Data are blocks for each channel LLL...LLLRRR...RRR or RRR...RRRLLL...LLL
    ptr := ABuffer;
    if ADataLayout = 'RRLL' then inc(ptr, ABufferSize div 2);
    AStream.WriteBuffer(ptr^, ABufferSize div 2);
    if ADataLayout = 'LLRR' then inc(ptr, ABufferSize div 2);
    AStream.WriteBuffer(ptr^, ABufferSize div 2);
  end else
    raise Exception.Create('Unknown WAVE data layout');

  // Complete missing header data
  AStream.Position := p - 4;
  AStream.WriteDWord(ABufferSize);  // Size of data part
  AStream.Position := 4;
  AStream.WriteDWord(AStream.Size - 8);
end;


end.


