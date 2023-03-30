use codespan_reporting::{
    diagnostic::{Label, Severity},
    term::{self, termcolor::WriteColor},
};
use comp_lib::diagnostic::{AggregateResult, Code, DiagnosticKind};
use is_terminal::IsTerminal;

pub fn eprint_aggregate<'files, T, F>(aggregate: &AggregateResult<T>, files: &'files F)
where
    F: codespan_reporting::files::Files<'files, FileId = ()>,
{
    let mut writer = if std::io::stderr().is_terminal() {
        term::termcolor::StandardStream::stderr(term::termcolor::ColorChoice::Always)
    } else {
        term::termcolor::StandardStream::stderr(term::termcolor::ColorChoice::Never)
    };

    let config = if writer.supports_color() {
        term::Config {
            chars: term::Chars {
                single_primary_caret: '━',
                single_secondary_caret: '╌',
                multi_primary_caret_start: '┚',
                multi_secondary_caret_start: '┘',
                multi_primary_caret_end: '┨',
                multi_secondary_caret_end: '┤',
                ..term::Chars::box_drawing()
            },
            ..Default::default()
        }
    } else {
        term::Config {
            chars: term::Chars::ascii(),
            ..Default::default()
        }
    };

    for (t, d) in aggregate.diagnostics() {
        let severity = match t {
            DiagnosticKind::Rec => Severity::Warning,
            DiagnosticKind::Err => Severity::Error,
        };

        let mut labels = Vec::with_capacity(1 + d.additional_spans_len());

        labels.push({
            let mut l = Label::primary((), *d.main_span());
            if let Some(m) = d.main_span_message() {
                l = l.with_message(m);
            }
            l
        });

        for (span, message) in d.additional_spans() {
            let mut l = Label::secondary((), *span);
            if let Some(m) = message {
                l = l.with_message(m);
            }
            labels.push(l);
        }

        let mut diagnostic = codespan_reporting::diagnostic::Diagnostic::new(severity)
            .with_message(d.message())
            .with_labels(labels);

        if d.code() != &Code::Unspecified {
            diagnostic = diagnostic.with_code(d.code().to_string())
        }

        term::emit(&mut writer, &config, files, &diagnostic).unwrap();
    }
}
