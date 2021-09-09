from ..log import log
from ...api import edn
from . import tap


def print_characters(view, characters):
    if characters is not None:
        view.run_command("append", {"characters": characters, "scroll_to_end": True})


def append_to_view(view, characters):
    if view and characters:
        view.set_read_only(False)
        print_characters(view, characters)
        view.set_read_only(True)
        view.run_command("move_to", {"to": "eof"})


def print_loop(view, client):
    try:
        log.debug({"event": "thread/start"})

        while item := client.printq.get():
            tag = item.get(edn.Keyword("tag"))
            val = item.get(edn.Keyword("val"))

            if tag == edn.Keyword("tap"):
                view.window().run_command("show_panel", {"panel": f"output.{tap.panel_name}"})
                panel = view.window().find_output_panel(tap.panel_name)
                append_to_view(panel, val)
            # Print invisible Unicode characters (U+2063) around stdout and
            # stderr to prevent them from getting syntax highlighting.
            #
            # This is probably somewhat evil, but the performance is *so*
            # much better than with view.add_regions.
            elif tag == edn.Keyword("err"):
                append_to_view(view, '⁣⁣' + val + '⁣⁣')
            elif tag == edn.Keyword("out"):
                append_to_view(view, '⁣' + val + '⁣')
            elif edn.Keyword("debug") in item:
                log.debug({"event": "info", "item": item.get(edn.Keyword("val"))})
            elif val := item.get(edn.Keyword("val")):
                # Babashka
                if not val.endswith("\n"):
                    val = val + "\n"

                append_to_view(view, val)
    finally:
        log.debug({"event": "thread/exit"})
