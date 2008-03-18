using System;
using System.Collections.Generic;

namespace Intel400x
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
            onup();
        }
        public void down()
        {
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
        private IEnumerable<Pin> pins;
        private string _name;

        public PinsGroup(string name, IEnumerable<Pin> pins)
        {
            this._name = name;
            this.pins = pins;
        }

        public string name
        {
            get { return _name; }
        }
        public void latch(int value)
        {
            foreach (Pin pin in pins)
            {
                pin.latch((value & 1) != 0);
                value >>= 1;
            }
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
    }
}
