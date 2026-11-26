package ffi

import (
	"errors"
	"slices"

	tea "charm.land/bubbletea/v2"
	"charm.land/huh/v2"
)

// Fields write their answers through pointers. Each field below keeps its
// pointer next to it, so PureScript reads the answer once the form is done.

type Entry struct {
	field     huh.Field
	pageBreak bool
}

type StringField struct {
	entry Entry
	value *string
}

type ChoiceField struct {
	entry Entry
	value *int
}

type ChoicesField struct {
	entry Entry
	value *[]int
}

type BoolField struct {
	entry Entry
	value *bool
}

type NativeForm = *huh.Form

var PageBreak = Entry{pageBreak: true}

// validator turns a PureScript check ("" for fine) into huh's. The check is
// pure, so it runs on whichever goroutine huh validates on.
func validator(check func(string) string) func(string) error {
	return func(value string) error {
		if problem := check(value); problem != "" {
			return errors.New(problem)
		}
		return nil
	}
}

func InputImpl(title, description, placeholder string, charLimit int, password bool, check func(string) string) StringField {
	value := new(string)
	field := huh.NewInput().Title(title).Description(description).Placeholder(placeholder).
		CharLimit(charLimit).Value(value).Validate(validator(check))
	if password {
		field = field.EchoMode(huh.EchoModePassword)
	}
	return StringField{entry: Entry{field: field}, value: value}
}

func TextImpl(title, description, placeholder string, lines, charLimit int, check func(string) string) StringField {
	value := new(string)
	field := huh.NewText().Title(title).Description(description).Placeholder(placeholder).
		Lines(lines).CharLimit(charLimit).Value(value).Validate(validator(check))
	return StringField{entry: Entry{field: field}, value: value}
}

func options(labels []string) []huh.Option[int] {
	choices := make([]huh.Option[int], len(labels))
	for i, label := range labels {
		choices[i] = huh.NewOption(label, i)
	}
	return choices
}

func SelectImpl(title, description string, labels []string, height int) ChoiceField {
	value := new(int)
	field := huh.NewSelect[int]().Title(title).Description(description).Options(options(labels)...).Value(value)
	if height > 0 {
		field = field.Height(height)
	}
	return ChoiceField{entry: Entry{field: field}, value: value}
}

func MultiSelectImpl(title, description string, labels []string, limit int) ChoicesField {
	value := new([]int)
	field := huh.NewMultiSelect[int]().Title(title).Description(description).Options(options(labels)...).Value(value)
	if limit > 0 {
		field = field.Limit(limit)
	}
	return ChoicesField{entry: Entry{field: field}, value: value}
}

func ConfirmImpl(title, description, affirmative, negative string, initial bool) BoolField {
	value := new(bool)
	*value = initial
	field := huh.NewConfirm().Title(title).Description(description).Affirmative(affirmative).Negative(negative).Value(value)
	return BoolField{entry: Entry{field: field}, value: value}
}

func NoteImpl(title, description string) Entry {
	return Entry{field: huh.NewNote().Title(title).Description(description)}
}

func StringEntry(f StringField) Entry   { return f.entry }
func ChoiceEntry(f ChoiceField) Entry   { return f.entry }
func ChoicesEntry(f ChoicesField) Entry { return f.entry }
func BoolEntry(f BoolField) Entry       { return f.entry }

func ReadStringImpl(f StringField) string { return *f.value }
func ReadChoiceImpl(f ChoiceField) int    { return *f.value }
func ReadBoolImpl(f BoolField) bool       { return *f.value }

func ReadChoicesImpl(f ChoicesField) []int { return slices.Clone(*f.value) }

func themeOf(theme int) huh.Theme {
	switch theme {
	case 1:
		return huh.ThemeFunc(huh.ThemeDracula)
	case 2:
		return huh.ThemeFunc(huh.ThemeBase16)
	case 3:
		return huh.ThemeFunc(huh.ThemeCatppuccin)
	case 4:
		return huh.ThemeFunc(huh.ThemeBase)
	}
	return huh.ThemeFunc(huh.ThemeCharm)
}

func FormImpl(entries []Entry, theme int, showHelp bool) *huh.Form {
	groups := []*huh.Group{}
	fields := []huh.Field{}
	flush := func() {
		if len(fields) > 0 {
			groups = append(groups, huh.NewGroup(fields...))
			fields = []huh.Field{}
		}
	}
	for _, entry := range entries {
		if entry.pageBreak {
			flush()
		} else {
			fields = append(fields, entry.field)
		}
	}
	flush()
	return huh.NewForm(groups...).WithTheme(themeOf(theme)).WithShowHelp(showHelp)
}

// RunImpl runs the form on a program of its own and waits until it's done.
func RunImpl[R any](completed R, aborted R, failed func(string) R, form *huh.Form) R {
	err := form.Run()
	switch {
	case err == nil:
		return completed
	case errors.Is(err, huh.ErrUserAborted):
		return aborted
	}
	return failed(err.Error())
}

func InitImpl(form *huh.Form) tea.Cmd { return form.Init() }

func UpdateImpl(form *huh.Form, msg tea.Msg) tea.Cmd {
	if msg == nil {
		return nil
	}
	_, cmd := form.Update(msg)
	return cmd
}

func ViewImpl(form *huh.Form) string { return form.View() }

// StateImpl: 0 filling in, 1 completed, 2 aborted.
func StateImpl(form *huh.Form) int {
	switch form.State {
	case huh.StateCompleted:
		return 1
	case huh.StateAborted:
		return 2
	}
	return 0
}
