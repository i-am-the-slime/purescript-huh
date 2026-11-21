package huh

import (
	"github.com/charmbracelet/huh"
	. "github.com/purescript-native/go-runtime"
)

func init() {
	exports := Foreign("Huh")

	exports["runInputImpl"] = func(opts_, callback_ Any) Any {
		return func() Any {
			opts := opts_.(Dict)
			callback := callback_.(func(Any) Any)
			go func() {
				var value string
				input := huh.NewInput().
					Title(opts["title"].(string)).
					Prompt(opts["prompt"].(string)).
					Placeholder(opts["placeholder"].(string)).
					Value(&value)
				err := input.Run()
				if err != nil {
					callback(Dict{"tag": "Left", "value0": err.Error()}).(func() Any)()
					return
				}
				callback(Dict{"tag": "Right", "value0": value}).(func() Any)()
			}()
			return nil
		}
	}

	exports["runConfirmImpl"] = func(opts_, callback_ Any) Any {
		return func() Any {
			opts := opts_.(Dict)
			callback := callback_.(func(Any) Any)
			go func() {
				var value bool
				c := huh.NewConfirm().
					Title(opts["title"].(string)).
					Affirmative(opts["affirmative"].(string)).
					Negative(opts["negative"].(string)).
					Value(&value)
				err := c.Run()
				if err != nil {
					callback(Dict{"tag": "Left", "value0": err.Error()}).(func() Any)()
					return
				}
				callback(Dict{"tag": "Right", "value0": value}).(func() Any)()
			}()
			return nil
		}
	}

	exports["runSelectImpl"] = func(opts_, callback_ Any) Any {
		return func() Any {
			opts := opts_.(Dict)
			callback := callback_.(func(Any) Any)
			go func() {
				options := opts["options"].([]Any)
				huhOpts := make([]huh.Option[string], len(options))
				for i, o := range options {
					s := o.(string)
					huhOpts[i] = huh.NewOption(s, s)
				}
				var value string
				s := huh.NewSelect[string]().
					Title(opts["title"].(string)).
					Options(huhOpts...).
					Value(&value)
				err := s.Run()
				if err != nil {
					callback(Dict{"tag": "Left", "value0": err.Error()}).(func() Any)()
					return
				}
				callback(Dict{"tag": "Right", "value0": value}).(func() Any)()
			}()
			return nil
		}
	}
}
