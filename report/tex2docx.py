"""Convert report.tex (this project's own constructs only) into a styled .docx."""
import re, matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
from docx import Document
from docx.shared import Pt, Inches, RGBColor, Mm
from docx.enum.text import WD_ALIGN_PARAGRAPH, WD_BREAK
from docx.enum.table import WD_TABLE_ALIGNMENT
from docx.oxml.ns import qn
from docx.oxml import OxmlElement

SRC = open('report.tex').read()
body = SRC[SRC.index(r'\begin{document}'):SRC.index(r'\end{document}')]

D = Document()
sec = D.sections[0]
sec.page_height, sec.page_width = Mm(297), Mm(210)
sec.top_margin = sec.bottom_margin = Inches(1.1)
sec.left_margin, sec.right_margin = Inches(1.15), Inches(1.0)
# page border like the template
pg = OxmlElement('w:pgBorders'); pg.set(qn('w:offsetFrom'), 'page')
for side in ('top', 'left', 'bottom', 'right'):
    e = OxmlElement(f'w:{side}'); e.set(qn('w:val'), 'single'); e.set(qn('w:sz'), '6')
    e.set(qn('w:space'), '24'); e.set(qn('w:color'), '000000'); pg.append(e)
sec._sectPr.append(pg)
# page numbers centred in footer
fp = sec.footer.paragraphs[0]; fp.alignment = WD_ALIGN_PARAGRAPH.CENTER
for t, v in [('begin', None), (None, 'PAGE'), ('end', None)]:
    r = fp.add_run()
    if t:
        f = OxmlElement('w:fldChar'); f.set(qn('w:fldCharType'), t); r._r.append(f)
    else:
        it = OxmlElement('w:instrText'); it.text = v; r._r.append(it)
sec.different_first_page_header_footer = True

FONT = 'Latin Modern Roman'
def setfont(style, name, size):
    style.font.name = name; style.font.size = Pt(size)
    style.element.rPr.rFonts.set(qn('w:eastAsia'), name)
    style.element.rPr.rFonts.set(qn('w:cs'), name)
st = D.styles['Normal']; setfont(st, 'Times New Roman', 12)
st.paragraph_format.line_spacing = 1.3; st.paragraph_format.space_after = Pt(6)
for name, size in [('Heading 1', 22), ('Heading 2', 15), ('Heading 3', 13)]:
    hs = D.styles[name]; setfont(hs, 'Times New Roman', size)
    hs.font.bold = True; hs.font.color.rgb = RGBColor(0, 0, 0); hs.font.italic = False
    hs.paragraph_format.space_before = Pt(14); hs.paragraph_format.space_after = Pt(8)
    hs.paragraph_format.keep_with_next = True

counters = {'ch': 0, 'sec': 0, 'sub': 0, 'fig': 0, 'tab': 0, 'eq': 0}

# ---------- inline LaTeX -> unicode ----------
SUP = str.maketrans('0123456789+-', '⁰¹²³⁴⁵⁶⁷⁸⁹⁺⁻')
SUB = str.maketrans('0123456789k', '₀₁₂₃₄₅₆₇₈₉ₖ')
def math_inline(m):
    s = m
    rep = [(r'\le', '≤'), (r'\ge', '≥'), (r'\pm', '±'), (r'\times', '×'), (r'\alpha', 'α'), (r'\beta', 'β'),
           (r'\varepsilon', 'ε'), (r'\bar d', 'd̄'), (r'\dots', '…'), (r'\cdots', '⋯'), (r'\,', ' '), ('~', ' ')]
    for a, b in rep: s = s.replace(a, b)
    s = re.sub(r'\\text\{([^}]*)\}', r'\1', s)
    s = re.sub(r'\^\{?([0-9+-]+)\}?', lambda x: x.group(1).translate(SUP), s)
    s = re.sub(r'_\{?([0-9k])\}?', lambda x: x.group(1).translate(SUB), s)
    s = re.sub(r'_\{([^}]*)\}', r'\1', s)
    s = s.replace('_', '').replace('{', '').replace('}', '')
    return s

def runs_from(text):
    """yield (text, bold, italic, color) segments from a LaTeX paragraph."""
    text = text.replace('\\\\', '\n').replace('---', '—').replace('--', '–').replace("``", '“').replace("''", '”')
    text = text.replace(r'\%', '%').replace(r'\&', '&').replace(r'\_', '_').replace(r'\#', '#').replace('~', ' ')
    text = re.sub(r'\\noindent\s*', '', text)
    out = []
    pos = 0
    pat = re.compile(r'\\textbf\{((?:[^{}]|\{[^{}]*\})*)\}|\\textit\{((?:[^{}]|\{[^{}]*\})*)\}|\$([^$]+)\$')
    for m in pat.finditer(text):
        if m.start() > pos: out.append((text[pos:m.start()], False, False))
        if m.group(1) is not None:
            for t, b, i in runs_from(m.group(1)): out.append((t, True, i))
        elif m.group(2) is not None:
            for t, b, i in runs_from(m.group(2)): out.append((t, b, True))
        else:
            out.append((math_inline(m.group(3)), False, False))
        pos = m.end()
    if pos < len(text): out.append((text[pos:], False, False))
    return [(re.sub(r'\s+', ' ', t) if '\n' not in t else t, b, i) for t, b, i in out]

def add_par(text, align=WD_ALIGN_PARAGRAPH.JUSTIFY, style=None, size=None, indent_first=False):
    p = D.add_paragraph(style=style) if style else D.add_paragraph()
    p.alignment = align
    if indent_first: p.paragraph_format.first_line_indent = Inches(0.3)
    for t, b, i in runs_from(text.strip()):
        r = p.add_run(t); r.bold = b; r.italic = i
        if size: r.font.size = Pt(size)
    return p

# ---------- display equations as images ----------
def eq_image(tex):
    counters['eq'] += 1
    t = tex.strip()
    t = re.sub(r'\\text\{([^}]*)\}', lambda m: r'\mathrm{' + m.group(1).replace(' ', r'\ ') + '}', t)
    t = t.replace(r'\tfrac', r'\frac').replace(r'\qquad', r'\quad\quad').replace(r'\cdots', r'\dots')
    fig = plt.figure(figsize=(0.01, 0.01))
    fig.text(0, 0, f'${t}$', fontsize=13, family='serif')
    path = f'eq_{counters["eq"]}.png'
    plt.rcParams['mathtext.fontset'] = 'cm'
    fig.savefig(path, dpi=300, bbox_inches='tight', pad_inches=0.04, transparent=True); plt.close(fig)
    from PIL import Image
    w, h = Image.open(path).size
    p = D.add_paragraph(); p.alignment = WD_ALIGN_PARAGRAPH.CENTER
    p.add_run().add_picture(path, width=Inches(min(w / 300, 6.0)))

# ---------- code boxes ----------
def shade(cell, color):
    tcPr = cell._tc.get_or_add_tcPr(); s = OxmlElement('w:shd')
    s.set(qn('w:val'), 'clear'); s.set(qn('w:color'), 'auto'); s.set(qn('w:fill'), color); tcPr.append(s)

def code_box(text, size):
    t = D.add_table(rows=1, cols=1); t.alignment = WD_TABLE_ALIGNMENT.CENTER
    c = t.rows[0].cells[0]; shade(c, '000000'); c.text = ''
    p = c.paragraphs[0]; p.paragraph_format.line_spacing = 1.0; p.paragraph_format.space_after = Pt(0)
    lines = text.strip('\n').split('\n')
    for k, line in enumerate(lines):
        # colour strings yellow like the template
        for j, seg in enumerate(re.split(r"('[^']*'|\"[^\"]*\")", line)):
            if not seg: continue
            r = p.add_run(seg); r.font.name = 'Courier New'; r.font.size = Pt(size)
            r._element.rPr.rFonts.set(qn('w:eastAsia'), 'Courier New')
            r.font.color.rgb = RGBColor(240, 220, 40) if (seg[0] in "'\"" and len(seg) > 1) else RGBColor(255, 255, 255)
        if k < len(lines) - 1: p.add_run().add_break()
    D.add_paragraph().paragraph_format.space_after = Pt(0)

# ---------- tables ----------
def borders(cell, top=None, bottom=None):
    tcPr = cell._tc.get_or_add_tcPr(); b = OxmlElement('w:tcBorders')
    for side, v in (('top', top), ('bottom', bottom)):
        if v:
            e = OxmlElement(f'w:{side}'); e.set(qn('w:val'), 'single'); e.set(qn('w:sz'), v); b.append(e)
    tcPr.append(b)

def table_block(block):
    tail = block.split('\\end{tabular}')[1].replace('\\end{table}', '').strip()
    cap = re.search(r'\\caption\{(.*)\}\s*$', tail, re.S)
    cap = cap.group(1) if cap else ''
    tab = re.search(r'\\begin\{tabular\}\{([^}]*)\}(.*)\\end\{tabular\}', block, re.S)
    spec, inner = tab.group(1), tab.group(2)
    rows, mids = [], set()
    for chunk in inner.split('\\\\'):
        chunk = chunk.strip()
        if r'\midrule' in chunk and rows: mids.add(len(rows))
        chunk = re.sub(r'\\(toprule|midrule|bottomrule)', '', chunk).strip()
        if chunk: rows.append([c.strip() for c in chunk.split('&')])
    ncol = len(rows[0])
    t = D.add_table(rows=len(rows), cols=ncol); t.alignment = WD_TABLE_ALIGNMENT.CENTER
    for i, row in enumerate(rows):
        for j in range(ncol):
            cell = t.rows[i].cells[j]; cell.text = ''
            p = cell.paragraphs[0]; p.paragraph_format.space_after = Pt(0); p.paragraph_format.line_spacing = 1.0
            al = spec.replace('|', '')[j] if j < len(spec.replace('|', '')) else 'l'
            p.alignment = {'l': WD_ALIGN_PARAGRAPH.LEFT, 'c': WD_ALIGN_PARAGRAPH.CENTER, 'r': WD_ALIGN_PARAGRAPH.RIGHT}.get(al, 0)
            for tx, b, it in runs_from(row[j] if j < len(row) else ''):
                r = p.add_run(tx); r.bold = b; r.italic = it; r.font.size = Pt(11)
            top = '12' if i == 0 else ('6' if i in mids else None)
            bot = '6' if i == 0 else ('12' if i == len(rows) - 1 else None)
            borders(cell, top, bot)
    counters['tab'] += 1
    c = add_par(f"Table {counters['ch']}.{counters['tab']}: {cap}", align=WD_ALIGN_PARAGRAPH.CENTER)
    c.paragraph_format.space_before = Pt(4)

def figure(path, width, cap):
    p = D.add_paragraph(); p.alignment = WD_ALIGN_PARAGRAPH.CENTER; p.paragraph_format.keep_with_next = True
    p.add_run().add_picture(path, width=Inches(6.0 * float(width)))
    counters['fig'] += 1
    add_par(f"Figure {counters['ch']}.{counters['fig']}: {cap}", align=WD_ALIGN_PARAGRAPH.CENTER)

# ---------- cover page ----------
def cover():
    def line(text, size, bold=False, italic=False, color=None, before=0):
        p = D.add_paragraph(); p.alignment = WD_ALIGN_PARAGRAPH.CENTER
        p.paragraph_format.space_before = Pt(before); p.paragraph_format.space_after = Pt(2)
        r = p.add_run(text); r.bold = bold; r.italic = italic; r.font.size = Pt(size)
        if color: r.font.color.rgb = RGBColor(*color)
    blue = (80, 80, 230)
    line('EVALUATING THE PREDICTIVE ACCURACY OF MULTIPLE LINEAR AND POLYNOMIAL REGRESSION MODELS IN ESTIMATING ACTIVE CALORIC EXPENDITURE', 16, True, before=6)
    line('A PROJECT REPORT', 13, True, before=10)
    line('Submitted to', 11, italic=True, before=22)
    line('MAHATMA GANDHI UNIVERSITY, KOTTAYAM', 13, True, before=4)
    line('In partial fulfilment of the requirements for the degree of', 11, italic=True, before=10)
    line('MASTER OF SCIENCE IN STATISTICS (APPLIED)', 13, True, before=4)
    line('By', 11, italic=True, before=20)
    line('ANEESHA K ANIL', 13, True, color=blue, before=2)
    line('(Reg. No: 240011019042)', 10)
    line('Under the guidance of', 11, italic=True, before=16)
    line('Dr. SMITHA S', 13, True, color=blue, before=2)
    line('Associate Professor', 11)
    p = D.add_paragraph(); p.alignment = WD_ALIGN_PARAGRAPH.CENTER; p.paragraph_format.space_before = Pt(14)
    p.add_run().add_picture('logo_11.png', width=Inches(1.05))
    line('DEPARTMENT OF STATISTICS', 13, True, before=20)
    line('KURIAKOSE ELIAS COLLEGE, MANNANAM', 13, True)
    line('KOTTAYAM, KERALA', 10)
    line('JUNE 2026', 13, True, before=12)
    D.add_paragraph().add_run().add_break(WD_BREAK.PAGE)

cover()

# ---------- walk the body ----------
main = body[body.index(r'\end{titlepage}') + len(r'\end{titlepage}'):]
main = re.sub(r'(?m)^%.*$', '', main)
main = main.replace(r'\setcounter{page}{1}', '')
tokens = re.split(r'(\\begin\{code\}.*?\\end\{code\}|\\begin\{out\}.*?\\end\{out\}|\\begin\{table\}\[H\].*?\\end\{table\}'
                  r'|\\begin\{(?:itemize|enumerate)\}.*?\\end\{(?:itemize|enumerate)\}|\\begin\{thebibliography\}.*?\\end\{thebibliography\}'
                  r'|\\fig\{[^}]*\}\{[^}]*\}\{(?:[^{}]|\{[^{}]*\})*\}|\\\[.*?\\\]'
                  r'|\\chapter\{[^}]*\}|\\section\*?\{(?:[^{}]|\{[^{}]*\})*\}|\\subsection\{(?:[^{}]|\{[^{}]*\})*\}|\\vspace\{[^}]*\})', main, flags=re.S)
first_chapter = True
for tk in tokens:
    if not tk or not tk.strip(): continue
    s = tk.strip()
    if s.startswith(r'\chapter'):
        counters['ch'] += 1; counters['sec'] = counters['fig'] = counters['tab'] = 0
        D.add_paragraph().add_run().add_break(WD_BREAK.PAGE)
        h = D.add_heading(f"Chapter {counters['ch']}", 1); h.paragraph_format.space_after = Pt(4)
        D.add_heading(s[9:-1], 1)
    elif s.startswith(r'\section*'):
        D.add_heading(s[10:-1], 2)
    elif s.startswith(r'\section'):
        counters['sec'] += 1; counters['sub'] = 0
        title = ''.join(t for t, _, _ in runs_from(s[9:-1]))
        D.add_heading(f"{counters['ch']}.{counters['sec']}   {title}", 2)
    elif s.startswith(r'\subsection'):
        counters['sub'] += 1
        title = ''.join(t for t, _, _ in runs_from(s[12:-1]))
        D.add_heading(f"{counters['ch']}.{counters['sec']}.{counters['sub']}   {title}", 3)
    elif s.startswith(r'\begin{code}'):
        code_box(s[len(r'\begin{code}'):-len(r'\end{code}')], 9)
    elif s.startswith(r'\begin{out}'):
        code_box(s[len(r'\begin{out}'):-len(r'\end{out}')], 6.5)
    elif s.startswith(r'\begin{table}'):
        table_block(s)
    elif s.startswith(r'\fig'):
        m = re.match(r'\\fig\{([^}]*)\}\{([^}]*)\}\{(.*)\}$', s, re.S)
        figure(m.group(1), m.group(2), ''.join(t for t, _, _ in runs_from(m.group(3))))
    elif s.startswith(r'\['):
        eq_image(s[2:-2])
    elif s.startswith(r'\begin{itemize}') or s.startswith(r'\begin{enumerate}'):
        style = 'List Bullet' if 'itemize' in s[:16] else 'List Number'
        inner = re.sub(r'^\\begin\{\w+\}(\[[^\]]*\])?|\\end\{\w+\}$', '', s).strip()
        for item in [x for x in inner.split(r'\item') if x.strip()]:
            add_par(item, style=style)
    elif s.startswith(r'\begin{thebibliography}'):
        D.add_paragraph().add_run().add_break(WD_BREAK.PAGE)
        D.add_heading('Bibliography', 1)
        items = re.split(r'\\bibitem\{[^}]*\}', s.split('}', 2)[2].replace(r'\end{thebibliography}', ''))
        for k, it in enumerate([x for x in items if x.strip()], 1):
            p = add_par(f'[{k}]  ' + it.strip(), align=WD_ALIGN_PARAGRAPH.LEFT)
            p.paragraph_format.left_indent = Inches(0.4); p.paragraph_format.first_line_indent = Inches(-0.4)
    elif s.startswith(r'\vspace'):
        continue
    else:
        for para in re.split(r'\n\s*\n', s):
            para = para.strip()
            para = re.sub(r'\\(sloppy|centering)\b', '', para).strip()
            if para: add_par(para)

D.save('Aneesha_Project_Report.docx')
print('ok', counters)
