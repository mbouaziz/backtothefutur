using System;
using System.Collections.Generic;

namespace Electronics
{
    public enum PullType
    {
        PullUp, PullDown, NoPull
    }

    public delegate void SignalEvent();

    public class Pin
    {
        private string _name;
        private Signal _signal;
        private PullType _pulltype;

        public event SignalEvent onup, ondown;

        public Pin(string name, Signal signal, PullType pulltype)
        {
            this._name = name;
            this._signal = signal;
            this._pulltype = pulltype;
            signal.plug(this);
        }

        public Pin(string name, Signal signal, PullType pulltype, SignalEvent onup, SignalEvent ondown)
        {
            this._name = name;
            this._signal = signal;
            this._pulltype = pulltype;
            this.onup += onup;
            this.ondown += ondown;
            signal.plug(this);
        }

        public PullType pulltype
        {
            get { return _pulltype; }
        }
        public string name
        {
            get { return _name; }
        }
        public Signal signal
        {
            get { return _signal; }
        }
        public void up()
        {
            if (onup != null)
                onup();
        }
        public void down()
        {
            if (ondown != null)
                ondown();
        }
        public void latch(bool value)
        {
            _signal.latch(this, value);
        }
        public void unlatch()
        {
            _signal.unlatch(this);
        }
        public bool value
        {
            get { return signal.value; }
        }
    }

    public class PinsGroup
    {
        private ICollection<Pin> pins;
        private string name;

        public PinsGroup(string name, ICollection<Pin> pins)
        {
            this.name = name;
            this.pins = pins;
        }
        public PinsGroup(string name, Pin[] pins, int i1, int i2)
        {
            int k = Math.Abs(i1 - i2);
            Pin[] pinsGroup = new Pin[k + 1];
            for (int i = i2; k >= 0; k--)
            {
                pinsGroup[k] = pins[i];
                i += i < i1 ? 1 : -1;
            }
            this.name = name;
            this.pins = pins;
        }

        public string Name
        {
            get { return name; }
        }
        public void latch(int value)
        {
            foreach (Pin pin in pins)
            {
                pin.latch((value & 1) != 0);
                value >>= 1;
            }
        }
        public void latch(string value)
        {
            int m = Math.Min(value.Length, pins.Count);
            int i = 0;
            foreach (Pin pin in pins)
                pin.latch(value[i++] == '1');
        }
        public void latch(byte[] value)
        {
            int m = Math.Min(value.Length, pins.Count);
            int i = 0;
            foreach (Pin pin in pins)
                pin.latch(value[i++] == (byte)'1');
        }
        public void unlatch()
        {
            foreach (Pin pin in pins)
                pin.unlatch();
        }
        public int value
        {
            get {
                int r = 0;
                int p = 1;
                foreach (Pin pin in pins)
                {
                    r |= pin.value ? p : 0;
                    p <<= 1;
                }
                return r;
            }
        }
        public byte[] valueBytesN
        {
            get
            {
                byte[] res = new byte[pins.Count+1];
                int i = 0;
                foreach (Pin pin in pins)
                    res[i++] = (byte)(pin.value ? '1' : '0');
                res[i] = (byte)'\n';
                return res;
            }
        }
        public string valueString
        {
            get
            {
                char[] res = new char[pins.Count];
                int i = 0;
                foreach (Pin pin in pins)
                    res[i++] = pin.value ? '1' : '0';
                return new string(res);
            }
        }
    }
}
