using System;
using System.Collections;
using System.Collections.Generic;

namespace Electronics
{
    public class Signal
    {
        private bool _value;
        private string _name;
        private PullType _pulltype;
        private ICollection<Pin> plugged;
        private ICollection<Pin> latched;

        public Signal(string name, PullType pulltype)
        {
            this._name = name;
            this._pulltype = pulltype;
            this.plugged = new List<Pin>();
            this.latched = new List<Pin>();
            this.value = (pulltype == PullType.PullUp);
        }

        public bool value
        {
            get {
                return this._value;
            }
            private set {
                if (value != _value)
                {   
                    _value = value;
                    foreach (Pin pin in plugged)
                        if (value)
                            pin.up();
                        else
                            pin.down();
                }
            }
        }

        public PullType pulltype
        {
            get { return _pulltype; }
        }
        public string name
        {
            get { return _name; }
        }

        public void plug(Pin pin)
        {
            if (pulltype == PullType.NoPull)
                _pulltype = pin.pulltype;
            plugged.Add(pin);
        }

        public void latch(Pin pin, bool value)
        {
            if (!latched.Contains(pin))
                latched.Add(pin);
            if (value != this.value)
            {
                this.value = value;
                if (latched.Count > 1)
                    throw new Exception("Error while trying to latch signal " + this.name + " with pin " + pin.name + " of value " + value);
            }
        }

        public void unlatch(Pin pin)
        {
            latched.Remove(pin);
            if (latched.Count == 0)
            {
                if (pulltype == PullType.PullDown)
                    value = false;
                else if (pulltype == PullType.PullUp)
                    value = true;
            }
        }
    }
}
