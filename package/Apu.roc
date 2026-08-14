import /Cartridge

# The 2A03 APU: two pulses, triangle, noise, DMC, frame counter, $4015.
# Pure state-in/state-out, like the Ppu. The console ticks it once per CPU
# cycle; the frame sequencer is specced in APU cycles (2 CPU cycles each)
# whose fractional boundaries land on integral CPU-cycle counts, so plain
# CPU-cycle counting stays exact. Mixing uses the non-linear approximation
# formulas into mono F32 at 48 kHz - the roc-web audio sink's rate - via a
# fractional accumulator; frontends drain the buffer with take_samples and
# adapt the channel count to their sink (the NES is mono).
#
# DMC sample fetches read PRG through the Cartridge (the same pattern as
# Ppu.tick) and report CPU stall cycles for the bus's stall accounting.

Env : {
    start : Bool,
    divider : U8,
    decay : U8,
    param : U8, # envelope divider period, and the volume when const_vol
    const_vol : Bool,
}

Pulse : {
    enabled : Bool,
    duty : U8,
    halt : Bool, # doubles as the envelope loop flag
    env : Env,
    sweep_on : Bool,
    sweep_period : U8,
    sweep_negate : Bool,
    sweep_shift : U8,
    sweep_div : U8,
    sweep_reload : Bool,
    ones : Bool, # pulse 1 negates by ones' complement, pulse 2 by two's
    period : U16,
    timer : U16,
    seq : U8,
    length : U8,
}

Triangle : {
    enabled : Bool,
    control : Bool, # halt flag
    reload_value : U8,
    linear : U8,
    reload_flag : Bool,
    period : U16,
    timer : U16,
    seq : U8,
    length : U8,
}

Noise : {
    enabled : Bool,
    halt : Bool,
    env : Env,
    short_mode : Bool,
    period : U16,
    timer : U16,
    lfsr : U16,
    length : U8,
}

Dmc : {
    enabled : Bool,
    irq_on : Bool,
    loop_flag : Bool,
    period : U16,
    timer : U16,
    level : U8, # 7-bit output
    sample_addr : U16,
    sample_len : U16,
    current_addr : U16,
    bytes_remaining : U16,
    buffer : [Empty, Full(U8)], # one-byte sample buffer, refilled as soon as it empties
    shift : U8,
    bits_remaining : U8,
    silence : Bool,
    irq_flag : Bool,
}

Frame : {
    mode5 : Bool,
    inhibit : Bool,
    c : I64, # CPU cycles into the sequence; negative right after a $4017 write
    irq_flag : Bool,
}

Apu := {
    pulse1 : Pulse,
    pulse2 : Pulse,
    triangle : Triangle,
    noise : Noise,
    dmc : Dmc,
    frame : Frame,
    odd : Bool, # pulse/half-rate timers clock every second CPU cycle
    sample_acc : U64,
    samples : List(F32),
}.{
    init : {} -> Apu
    init = |_| {
        env0 : Env
        env0 = { start: Bool.False, divider: 0, decay: 0, param: 0, const_vol: Bool.False }
        pulse0 : Pulse
        pulse0 = {
            enabled: Bool.False,
            duty: 0,
            halt: Bool.False,
            env: env0,
            sweep_on: Bool.False,
            sweep_period: 0,
            sweep_negate: Bool.False,
            sweep_shift: 0,
            sweep_div: 0,
            sweep_reload: Bool.False,
            ones: Bool.True,
            period: 0,
            timer: 0,
            seq: 0,
            length: 0,
        }
        {
            pulse1: pulse0,
            pulse2: { ..pulse0, ones: Bool.False },
            triangle: { enabled: Bool.False, control: Bool.False, reload_value: 0, linear: 0, reload_flag: Bool.False, period: 0, timer: 0, seq: 0, length: 0 },
            noise: { enabled: Bool.False, halt: Bool.False, env: env0, short_mode: Bool.False, period: 4, timer: 0, lfsr: 1, length: 0 },
            dmc: { enabled: Bool.False, irq_on: Bool.False, loop_flag: Bool.False, period: 428, timer: 0, level: 0, sample_addr: 0xC000, sample_len: 1, current_addr: 0xC000, bytes_remaining: 0, buffer: Empty, shift: 0, bits_remaining: 8, silence: Bool.True, irq_flag: Bool.False },
            frame: { mode5: Bool.False, inhibit: Bool.False, c: 0, irq_flag: Bool.False },
            odd: Bool.False,
            sample_acc: 0,
            samples: [],
        }
    }

    length_load : U8 -> U8
    length_load = |idx| {
        t = [10, 254, 20, 2, 40, 4, 80, 6, 160, 8, 60, 10, 14, 12, 26, 14, 12, 16, 24, 18, 48, 20, 96, 22, 192, 24, 72, 26, 16, 28, 32, 30]
        t.get(idx.bitwise_and(0x1F).to_u64()) ?? 0
    }

    noise_period : U8 -> U16
    noise_period = |idx| {
        t = [4, 8, 16, 32, 64, 96, 128, 160, 202, 254, 380, 508, 762, 1016, 2034, 4068]
        t.get(idx.bitwise_and(0x0F).to_u64()) ?? 4
    }

    dmc_rate : U8 -> U16
    dmc_rate = |idx| {
        t = [428, 380, 340, 320, 286, 254, 226, 214, 190, 160, 142, 128, 106, 84, 72, 54]
        t.get(idx.bitwise_and(0x0F).to_u64()) ?? 428
    }

    # duty sequences, one byte per duty; bit n is the waveform at step n
    duty_bit : U8, U8 -> Bool
    duty_bit = |duty, step| {
        masks = [0b00000010, 0b00000110, 0b00011110, 0b11111001]
        m = masks.get(duty.bitwise_and(3).to_u64()) ?? 0.U8
        m.bitwise_and(U8.shl_wrap(1, step.bitwise_and(7))) != 0.U8
    }

    tri_level : U8 -> U8
    tri_level = |step| {
        t = [15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]
        t.get(step.bitwise_and(0x1F).to_u64()) ?? 0
    }

    # --- register writes ---

    pulse_write : Pulse, U16, U8 -> Pulse
    pulse_write = |p, r, v|
        if r == 0 {
            {
                ..p,
                duty: v.shr_zf_wrap(6),
                halt: v.bitwise_and(0x20) != 0,
                env: { ..p.env, const_vol: v.bitwise_and(0x10) != 0, param: v.bitwise_and(0x0F) },
            }
        } else if r == 1 {
            {
                ..p,
                sweep_on: v.bitwise_and(0x80) != 0,
                sweep_period: v.shr_zf_wrap(4).bitwise_and(7),
                sweep_negate: v.bitwise_and(0x08) != 0,
                sweep_shift: v.bitwise_and(7),
                sweep_reload: Bool.True,
            }
        } else if r == 2 {
            { ..p, period: p.period.bitwise_and(0x0700).bitwise_or(v.to_u16()) }
        } else {
            len = if p.enabled { length_load(v.shr_zf_wrap(3)) } else { p.length }
            {
                ..p,
                period: p.period.bitwise_and(0x00FF).bitwise_or(v.bitwise_and(7).to_u16().shl_wrap(8)),
                length: len,
                seq: 0,
                env: { ..p.env, start: Bool.True },
            }
        }

    write_reg : Apu, U16, U8 -> Apu
    write_reg = |apu, addr, v|
        if addr <= 0x4003 {
            { ..apu, pulse1: pulse_write(apu.pulse1, addr.bitwise_and(3), v) }
        } else if addr <= 0x4007 {
            { ..apu, pulse2: pulse_write(apu.pulse2, addr.bitwise_and(3), v) }
        } else if addr == 0x4008 {
            { ..apu, triangle: { ..apu.triangle, control: v.bitwise_and(0x80) != 0, reload_value: v.bitwise_and(0x7F) } }
        } else if addr == 0x400A {
            { ..apu, triangle: { ..apu.triangle, period: apu.triangle.period.bitwise_and(0x0700).bitwise_or(v.to_u16()) } }
        } else if addr == 0x400B {
            t = apu.triangle
            len = if t.enabled { length_load(v.shr_zf_wrap(3)) } else { t.length }
            { ..apu, triangle: { ..t, period: t.period.bitwise_and(0x00FF).bitwise_or(v.bitwise_and(7).to_u16().shl_wrap(8)), length: len, reload_flag: Bool.True } }
        } else if addr == 0x400C {
            nz = apu.noise
            { ..apu, noise: { ..nz, halt: v.bitwise_and(0x20) != 0, env: { ..nz.env, const_vol: v.bitwise_and(0x10) != 0, param: v.bitwise_and(0x0F) } } }
        } else if addr == 0x400E {
            { ..apu, noise: { ..apu.noise, short_mode: v.bitwise_and(0x80) != 0, period: noise_period(v) } }
        } else if addr == 0x400F {
            nz = apu.noise
            len = if nz.enabled { length_load(v.shr_zf_wrap(3)) } else { nz.length }
            { ..apu, noise: { ..nz, length: len, env: { ..nz.env, start: Bool.True } } }
        } else if addr == 0x4010 {
            d = apu.dmc
            irq_on = v.bitwise_and(0x80) != 0
            { ..apu, dmc: { ..d, irq_on: irq_on, loop_flag: v.bitwise_and(0x40) != 0, period: dmc_rate(v), irq_flag: if irq_on { d.irq_flag } else { Bool.False } } }
        } else if addr == 0x4011 {
            { ..apu, dmc: { ..apu.dmc, level: v.bitwise_and(0x7F) } }
        } else if addr == 0x4012 {
            { ..apu, dmc: { ..apu.dmc, sample_addr: v.to_u16().shl_wrap(6).bitwise_or(0xC000) } }
        } else if addr == 0x4013 {
            { ..apu, dmc: { ..apu.dmc, sample_len: v.to_u16().shl_wrap(4).bitwise_or(1) } }
        } else if addr == 0x4015 {
            write_status(apu, v)
        } else if addr == 0x4017 {
            # the divider reset takes effect 3 cycles after the write on even
            # put-cycles, 4 on odd (blargg's jitter test); our APU also sees
            # the write a few cycles before its true position (register
            # writes land at the start of the instruction's cycle batch), so
            # the sequence starts a little in the past
            f : Frame
            f = {
                mode5: v.bitwise_and(0x80) != 0,
                inhibit: v.bitwise_and(0x40) != 0,
                c: if apu.odd { -3 } else { -2 },
                irq_flag: if v.bitwise_and(0x40) != 0 { Bool.False } else { apu.frame.irq_flag },
            }
            a2 = { ..apu, frame: f }
            # writing 5-step mode clocks quarter+half immediately
            if f.mode5 { clock_half(clock_quarter(a2)) } else { a2 }
        } else {
            apu
        }

    write_status : Apu, U8 -> Apu
    write_status = |apu, v| {
        en = |bit| v.bitwise_and(bit) != 0
        p1 = { ..apu.pulse1, enabled: en(0x01), length: if en(0x01) { apu.pulse1.length } else { 0 } }
        p2 = { ..apu.pulse2, enabled: en(0x02), length: if en(0x02) { apu.pulse2.length } else { 0 } }
        tr = { ..apu.triangle, enabled: en(0x04), length: if en(0x04) { apu.triangle.length } else { 0 } }
        nz = { ..apu.noise, enabled: en(0x08), length: if en(0x08) { apu.noise.length } else { 0 } }
        d0 = apu.dmc
        d =
            if en(0x10) {
                if d0.bytes_remaining == 0 {
                    { ..d0, enabled: Bool.True, current_addr: d0.sample_addr, bytes_remaining: d0.sample_len, irq_flag: Bool.False }
                } else {
                    { ..d0, enabled: Bool.True, irq_flag: Bool.False }
                }
            } else {
                { ..d0, enabled: Bool.False, bytes_remaining: 0, irq_flag: Bool.False }
            }
        { ..apu, pulse1: p1, pulse2: p2, triangle: tr, noise: nz, dmc: d }
    }

    # $4015 read: channel length activity + interrupt flags; acks the frame IRQ
    read_status : Apu -> { apu : Apu, value : U8 }
    read_status = |apu| {
        b = |cond, n| if cond { U8.shl_wrap(1, n) } else { 0 }
        v = b(apu.pulse1.length > 0, 0)
            .bitwise_or(b(apu.pulse2.length > 0, 1))
            .bitwise_or(b(apu.triangle.length > 0, 2))
            .bitwise_or(b(apu.noise.length > 0, 3))
            .bitwise_or(b(apu.dmc.bytes_remaining > 0, 4))
            .bitwise_or(b(apu.frame.irq_flag, 6))
            .bitwise_or(b(apu.dmc.irq_flag, 7))
        { apu: { ..apu, frame: { ..apu.frame, irq_flag: Bool.False } }, value: v }
    }

    # --- frame sequencer clocks ---

    env_clock : Env, Bool -> Env
    env_clock = |e, env_loop|
        if e.start {
            { ..e, start: Bool.False, decay: 15, divider: e.param }
        } else if e.divider == 0 {
            d = if e.decay > 0 { e.decay - 1 } else if env_loop { 15 } else { 0 }
            { ..e, divider: e.param, decay: d }
        } else {
            { ..e, divider: e.divider - 1 }
        }

    clock_quarter : Apu -> Apu
    clock_quarter = |apu| {
        t0 = apu.triangle
        t1 = if t0.reload_flag { { ..t0, linear: t0.reload_value } } else if t0.linear > 0 { { ..t0, linear: t0.linear - 1 } } else { t0 }
        t2 = if t1.control { t1 } else { { ..t1, reload_flag: Bool.False } }
        {
            ..apu,
            pulse1: { ..apu.pulse1, env: env_clock(apu.pulse1.env, apu.pulse1.halt) },
            pulse2: { ..apu.pulse2, env: env_clock(apu.pulse2.env, apu.pulse2.halt) },
            noise: { ..apu.noise, env: env_clock(apu.noise.env, apu.noise.halt) },
            triangle: t2,
        }
    }

    length_clock : U8, Bool -> U8
    length_clock = |len, halt| if halt or len == 0 { len } else { len - 1 }

    sweep_target : Pulse -> U16
    sweep_target = |p| {
        change = p.period.shr_zf_wrap(p.sweep_shift)
        if p.sweep_negate {
            sub = if p.ones { change.plus(1) } else { change }
            if p.period >= sub { p.period - sub } else { 0 }
        } else {
            p.period.plus(change)
        }
    }

    sweep_clock : Pulse -> Pulse
    sweep_clock = |p| {
        target = sweep_target(p)
        p2 =
            if p.sweep_div == 0 and p.sweep_on and p.sweep_shift != 0 and p.period >= 8 and target <= 0x07FF {
                { ..p, period: target }
            } else {
                p
            }
        if p2.sweep_div == 0 or p2.sweep_reload {
            { ..p2, sweep_div: p2.sweep_period, sweep_reload: Bool.False }
        } else {
            { ..p2, sweep_div: p2.sweep_div - 1 }
        }
    }

    clock_half : Apu -> Apu
    clock_half = |apu| {
        p1 = sweep_clock({ ..apu.pulse1, length: length_clock(apu.pulse1.length, apu.pulse1.halt) })
        p2 = sweep_clock({ ..apu.pulse2, length: length_clock(apu.pulse2.length, apu.pulse2.halt) })
        {
            ..apu,
            pulse1: p1,
            pulse2: p2,
            triangle: { ..apu.triangle, length: length_clock(apu.triangle.length, apu.triangle.control) },
            noise: { ..apu.noise, length: length_clock(apu.noise.length, apu.noise.halt) },
        }
    }

    # NTSC sequence, counted in CPU cycles (integral: each APU-cycle boundary
    # with a .5 doubles to a whole CPU cycle)
    frame_advance : Apu -> Apu
    frame_advance = |apu0| {
        c = apu0.frame.c.plus(1)
        a = { ..apu0, frame: { ..apu0.frame, c: c } }
        if c == 7457 or c == 22371 {
            clock_quarter(a)
        } else if c == 14913 {
            clock_quarter(a)
        } else if c == 14914 {
            # the half-frame clocks land one cycle after the quarter's, as
            # observed by blargg's len_timing
            clock_half(a)
        } else if a.frame.mode5 {
            if c == 37281 {
                clock_quarter(a)
            } else if c == 37282 {
                clock_half(a)
            } else if c >= 37283 {
                { ..a, frame: { ..a.frame, c: 1 } }
            } else {
                a
            }
        } else if c == 29829 {
            set_frame_irq(clock_quarter(a))
        } else if c == 29830 {
            set_frame_irq(clock_half(a))
        } else if c >= 29831 {
            # the flag is raised on three consecutive cycles; the sequence
            # period stays 29830 (cycle 29831 is cycle 1 of the next pass)
            a2 = set_frame_irq(a)
            { ..a2, frame: { ..a2.frame, c: 1 } }
        } else {
            a
        }
    }

    set_frame_irq : Apu -> Apu
    set_frame_irq = |apu|
        if apu.frame.inhibit {
            apu
        } else {
            { ..apu, frame: { ..apu.frame, irq_flag: Bool.True } }
        }

    # --- channel timers ---

    pulse_step : Pulse -> Pulse
    pulse_step = |p|
        if p.timer == 0 {
            { ..p, timer: p.period, seq: p.seq.plus(1).bitwise_and(7) }
        } else {
            { ..p, timer: p.timer - 1 }
        }

    tri_step : Triangle -> Triangle
    tri_step = |t|
        if t.timer == 0 {
            if t.length > 0 and t.linear > 0 {
                { ..t, timer: t.period, seq: t.seq.plus(1).bitwise_and(0x1F) }
            } else {
                { ..t, timer: t.period }
            }
        } else {
            { ..t, timer: t.timer - 1 }
        }

    noise_step : Noise -> Noise
    noise_step = |nz|
        if nz.timer == 0 {
            tap = if nz.short_mode { nz.lfsr.shr_zf_wrap(6) } else { nz.lfsr.shr_zf_wrap(1) }
            fb = nz.lfsr.bitwise_xor(tap).bitwise_and(1)
            { ..nz, timer: nz.period - 1, lfsr: nz.lfsr.shr_zf_wrap(1).bitwise_or(fb.shl_wrap(14)) }
        } else {
            { ..nz, timer: nz.timer - 1 }
        }

    # refill the one-byte sample buffer as soon as it is empty and bytes
    # remain: read PRG via the mapper, cost a CPU stall, and handle the
    # sample's end (loop or IRQ) at fetch time
    dmc_refill : Dmc, Cartridge -> { dmc : Dmc, stall : U64 }
    dmc_refill = |d, cart|
        match d.buffer {
            Full(_) => { dmc: d, stall: 0 }
            Empty =>
                if d.bytes_remaining == 0 {
                    { dmc: d, stall: 0 }
                } else {
                    byte = cart.read_prg(d.current_addr)
                    next_addr = if d.current_addr == 0xFFFF { 0x8000 } else { d.current_addr.plus(1) }
                    remaining = d.bytes_remaining - 1
                    d2 = { ..d, buffer: Full(byte), current_addr: next_addr, bytes_remaining: remaining }
                    d3 =
                        if remaining == 0 {
                            if d2.loop_flag {
                                { ..d2, current_addr: d2.sample_addr, bytes_remaining: d2.sample_len }
                            } else if d2.irq_on {
                                { ..d2, irq_flag: Bool.True }
                            } else {
                                d2
                            }
                        } else {
                            d2
                        }
                    { dmc: d3, stall: 4 }
                }
        }

    # DMC output unit: one CPU cycle
    dmc_step : Dmc, Cartridge -> { dmc : Dmc, stall : U64 }
    dmc_step = |d0, cart| {
        r = dmc_refill(d0, cart)
        d = r.dmc
        if d.timer == 0 {
            level =
                if d.silence {
                    d.level
                } else if d.shift.bitwise_and(1) != 0 {
                    if d.level <= 125 { d.level.plus(2) } else { d.level }
                } else {
                    if d.level >= 2 { d.level - 2 } else { d.level }
                }
            d1 = { ..d, timer: d.period - 1, level: level, shift: d.shift.shr_zf_wrap(1), bits_remaining: d.bits_remaining - 1 }
            if d1.bits_remaining == 0 {
                d2 =
                    match d1.buffer {
                        Full(b) => { ..d1, bits_remaining: 8, silence: Bool.False, shift: b, buffer: Empty }
                        Empty => { ..d1, bits_remaining: 8, silence: Bool.True }
                    }
                { dmc: d2, stall: r.stall }
            } else {
                { dmc: d1, stall: r.stall }
            }
        } else {
            { dmc: { ..d, timer: d.timer - 1 }, stall: r.stall }
        }
    }

    # --- mixing ---

    pulse_out : Pulse -> U8
    pulse_out = |p|
        if p.length > 0 and p.period >= 8 and sweep_target(p) <= 0x07FF and duty_bit(p.duty, p.seq) {
            if p.env.const_vol { p.env.param } else { p.env.decay }
        } else {
            0
        }

    noise_out : Noise -> U8
    noise_out = |nz|
        if nz.length > 0 and nz.lfsr.bitwise_and(1) == 0 {
            if nz.env.const_vol { nz.env.param } else { nz.env.decay }
        } else {
            0
        }

    mix : Apu -> F32
    mix = |apu| {
        psum = pulse_out(apu.pulse1).plus(pulse_out(apu.pulse2)).to_f32()
        pout : F32
        pout = if psum > 0.0 { 95.88 / ((8128.0 / psum) + 100.0) } else { 0.0 }
        t = tri_level(apu.triangle.seq).to_f32()
        n = noise_out(apu.noise).to_f32()
        d = apu.dmc.level.to_f32()
        tnd_sum : F32
        tnd_sum = (t / 8227.0) + (n / 12241.0) + (d / 22638.0)
        tnd : F32
        tnd = if tnd_sum > 0.0 { 159.79 / ((1.0 / tnd_sum) + 100.0) } else { 0.0 }
        pout + tnd
    }

    # --- console interface ---

    tick : Apu, Cartridge, U64 -> { apu : Apu, stall : U64, irq : Bool }
    tick = |apu0, cart, cycles| {
        var a = apu0
        var stall = 0.U64
        var i = 0.U64
        while i < cycles {
            a = frame_advance(a)
            a = { ..a, triangle: tri_step(a.triangle) }
            a = if a.odd { { ..a, pulse1: pulse_step(a.pulse1), pulse2: pulse_step(a.pulse2) } } else { a }
            a = { ..a, noise: noise_step(a.noise), odd: if a.odd { Bool.False } else { Bool.True } }
            ds = dmc_step(a.dmc, cart)
            a = { ..a, dmc: ds.dmc }
            stall = stall + ds.stall
            acc = a.sample_acc.plus(48000)
            a =
                if acc >= 1789773 {
                    # cap the buffer (~1.5 s) so a frontend that never
                    # drains (today's rocray app) holds steady memory
                    kept = if a.samples.len() < 65536 { a.samples.append(mix(a)) } else { a.samples }
                    { ..a, sample_acc: acc - 1789773, samples: kept }
                } else {
                    { ..a, sample_acc: acc }
                }
            i = i + 1
        }
        { apu: a, stall: stall, irq: a.frame.irq_flag or a.dmc.irq_flag }
    }

    take_samples : Apu -> { apu : Apu, samples : List(F32) }
    take_samples = |apu| { apu: { ..apu, samples: [] }, samples: apu.samples }
}

# --- expects (register effects; no cartridge needed except for tick) ---

# pulse period registers assemble an 11-bit period; $4015 enable + length
# load turn the status bit on (the pre-APU stub returned 0 here forever)
expect {
    apu = Apu.init({})
        .write_reg(0x4015, 0x01)
        .write_reg(0x4002, 0xAB)
        .write_reg(0x4003, 0x12) # period high 2, length index 2 -> 20
    r = apu.read_status()
    apu.pulse1.period == 0x02AB and apu.pulse1.length == 20 and r.value.bitwise_and(0x01) == 1
}

# writes with the channel disabled do not load the length counter
expect {
    apu = Apu.init({}).write_reg(0x4003, 0x12)
    r = apu.read_status()
    apu.pulse1.length == 0 and r.value.bitwise_and(0x01) == 0
}

# $4015 disable clears the length counter immediately
expect {
    apu = Apu.init({})
        .write_reg(0x4015, 0x01)
        .write_reg(0x4003, 0x12)
        .write_reg(0x4015, 0x00)
    apu.pulse1.length == 0
}

# length counter counts down at half-frame clocks and silences at zero;
# the halt flag freezes it
expect {
    apu = Apu.init({})
        .write_reg(0x4015, 0x08)
        .write_reg(0x400F, 0x18) # length index 3 -> 2
    one = Apu.clock_half(apu)
    two = Apu.clock_half(one)
    halted = Apu.clock_half(Apu.init({}).write_reg(0x4015, 0x08).write_reg(0x400C, 0x20).write_reg(0x400F, 0x18))
    one.noise.length == 1 and two.noise.length == 0 and halted.noise.length == 2
}

# $4017 bit 6 inhibits and acknowledges the frame IRQ
expect {
    apu = Apu.init({})
    with_flag = { ..apu, frame: { ..apu.frame, irq_flag: Bool.True } }
    cleared = with_flag.write_reg(0x4017, 0x40)
    cleared.frame.irq_flag == Bool.False
}
