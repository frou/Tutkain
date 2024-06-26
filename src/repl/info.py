import html
import inspect
import os
import pathlib
import re
import tempfile
from urllib.parse import urlparse
from urllib.request import url2pathname
from zipfile import ZipFile

import sublime

from ...api import edn


def goto(
    window,
    location,
    flags=sublime.ENCODED_POSITION,
):
    if location:
        resource = location["resource"]
        line = location["line"] + 1
        column = location["column"] + 1

        active_view = window.active_view()

        if (
            resource.path
            and active_view
            # The active view is a view into a resource in a JAR file
            and (
                resource.path
                == active_view.settings()
                .get("tutkain_temp_file", {})
                .get("resource_path")
            )
            # The active view is the same as the target view
            or (
                (file_name := active_view.file_name())
                and os.path.realpath(resource.path) == os.path.realpath(file_name)
            )
        ):
            point = active_view.text_point_utf8(line - 1, column - 1)

            active_view.run_command("tutkain_goto_point_impl", {"point": point})
        elif not resource.scheme or resource.scheme == "file" and resource.path:
            view = window.open_file(
                f"{resource.path}:{line}:{column}",
                flags=sublime.ENCODED_POSITION
                | sublime.SEMI_TRANSIENT
                | sublime.REPLACE_MRU,
            )
        elif resource.scheme == "jar" and "!" in resource.path:
            parts = resource.path.split("!")
            jar_url = urlparse(parts[0])
            # Strip the leading slash from the path on Windows
            jar_path = url2pathname(jar_url.path)
            # If the path after the ! starts with a forward slash, strip it. ZipFile can't
            # find the file inside the archive otherwise.
            if os.path.isfile(jar_path):
                path_after_bang = parts[1][1:] if parts[1].startswith("/") else parts[1]
                archive = ZipFile(jar_path, "r")
                zipinfo = archive.getinfo(path_after_bang)
                path = pathlib.Path(path_after_bang)
                descriptor, temp_path = tempfile.mkstemp(path.suffix)

                try:
                    path = pathlib.Path(temp_path)
                    zipinfo.filename = path.name
                    archive.extract(zipinfo, path.parent)
                    view = window.open_file(f"{path}:{line}:{column}", flags=flags)

                    view.settings().set(
                        "tutkain_temp_file",
                        {
                            "resource_path": resource.path,
                            "path": temp_path,
                            "descriptor": descriptor,
                            "name": f"$CLASSPATH/{path_after_bang}",
                        },
                    )

                    view.set_scratch(True)
                    view.set_read_only(True)
                except:
                    if os.path.exists(temp_path):
                        os.close(descriptor)
                        os.remove(temp_path)


def parse_location(info):
    if info and (file := info.get(edn.Keyword("file"))):
        return {
            "resource": urlparse(file),
            "line": int(info.get(edn.Keyword("line"), "1")) - 1,
            "column": int(info.get(edn.Keyword("column"), "1")) - 1,
        }


def htmlify(text):
    if text:
        return re.sub(r"\n", "<br/>", inspect.cleandoc(html.escape(text)))
    else:
        return ""


def show_popup(view, point, response):
    window = view.window() or sublime.active_window()

    if edn.Keyword("info") in response:
        info = response[edn.Keyword("info")]

        if info:
            file = info.get(edn.Keyword("file"), "")
            location = parse_location(info)
            ns = info.get(edn.Keyword("ns"), "clojure.core")
            name = info.get(edn.Keyword("name"), edn.Symbol("")).name
            arglists = info.get(edn.Keyword("arglists"), "")
            spec = info.get(edn.Keyword("spec"), "")
            fnspec = info.get(edn.Keyword("fnspec"), {})
            doc = info.get(edn.Keyword("doc"), "")

            if name and file:
                name = f"""
                <p class="name">
                    <a href="{file}">{html.escape(ns)}/{html.escape(name)}</a>
                </p>
                """
            elif name:
                name = f"""
                <p class="name">{html.escape(ns)}/{html.escape(name)}</p>
                """
            else:
                name = ""

            content = f"""
                <body id="tutkain-lookup">
                    <style>
                        #tutkain-lookup {{
                            font-size: .9rem;
                            padding: 0;
                            margin: 0;
                        }}

                        a {{
                            text-decoration: none;
                        }}

                        p {{
                            margin: 0;
                            padding: .25rem .5rem;
                        }}

                        .name, .arglists, .spec, .fnspec {{
                            border-bottom: 1px solid color(var(--foreground) alpha(0.05));
                        }}

                        .arglists, .spec, .fnspec {{
                            color: color(var(--foreground) alpha(0.5));
                        }}

                        .fnspec-key {{
                            color: color(var(--foreground) alpha(0.75));
                        }}
                    </style>
                    {name}"""

            if arglists:
                content += """<p class="arglists">"""

                for arglist in arglists:
                    content += f"""{htmlify(arglist)} """

                content += "</p>"

            if spec:
                content += f"""<p class="spec"><code>{htmlify(spec)}</code></p>"""

            if fnspec:
                content += """<p class="fnspec">"""

                for k in [edn.Keyword("args"), edn.Keyword("ret"), edn.Keyword("fn")]:
                    if k in fnspec:
                        content += f""":<span class="fnspec-key">{k.name}</span> {htmlify(fnspec[k])}<br/>"""

                content += """</p>"""

            if doc:
                doc = re.sub(r"\s", "&nbsp;", htmlify(doc))
                content += f"""<p class="doc">{doc}</p>"""

            content += "</body>"

            view.show_popup(
                content,
                location=point,
                max_width=1024,
                on_navigate=lambda href: goto(window, location),
                flags=sublime.COOPERATE_WITH_AUTO_COMPLETE,
            )
