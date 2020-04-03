#!/usr/bin/env python3
# coding: utf-8
"""
Author       : afronski
Created Time : 2020-03-30 21:58:12
Prerequisite:
    python3 -m pip install cson arrow
"""
import json
import os
import sys
import datetime
import cson
import re
import shutil
try:
    import arrow
    time_aware = True
except ImportError:
    print(
        'warning: datetime information will be discarded unless you install arrow'
    )
    time_aware = False


def read_file(fp):
    with open(fp) as f:
        return f.read()


def text_to_dict(text):
    """convert json or cson to dict"""
    try:
        return cson.loads(text)
    except:
        pass

    try:
        return json.loads(text)
    except:
        pass
    raise Exception("text should be in cson or json format")


def read_folder_names(fp):
    data = text_to_dict(read_file(fp))
    return {x['key']: x['name'] for x in data['folders']}


def sanitize(s):
    return re.sub(r'\-$', '', re.sub(r'^\-', '', re.sub(r'\-+', '-', re.sub(r'[^A-Za-zżółćęśąźń0-9\/\-]+', '-', s.lower()))))

def write_boostnote_markdown(data, output, folder_map, old_name, notes_dir):
    """write boostnote dict to markdown"""
    target_dir = os.path.join(output, folder_map[data['folder']].replace('/', '-'))
    target_dir = sanitize(target_dir)

    if not os.path.exists(target_dir):
        os.makedirs(target_dir)

    folder_name = data['title'].replace('/', '-')
    folder_name = sanitize(folder_name)

    target_file = os.path.join(target_dir, '{}.md'.format(folder_name))

    with open(target_file, 'w') as f:
        content = data.get('content', '')

        fmt = r'\:storage\/([A-Za-z0-9\-]+)/([A-Za-z0-9]+\.[A-Za-z0-9]+)'
        images = [m for m in re.finditer(fmt, content)]

        for i in images:
            images_dir = os.path.join(output, ".images", folder_name)
            if not os.path.exists(images_dir):
                os.makedirs(images_dir)

            di = i.group(1)
            fi = i.group(2)

            src = os.path.join(notes_dir, "attachments", di, fi)

            if os.path.exists(src):
                dst = os.path.join(images_dir, fi)

                shutil.copy(src, dst)

                relative_dst = "../.images/{}/{}".format(folder_name, fi)
                content = re.sub(fmt, relative_dst, content, 1)
            else:
                content = re.sub(fmt, "%MISSING_LINK%", content, 1)
                print("  - missing file: {}".format(src))

        f.write('---\ntags: {}\n---\n\n'.format(', '.join(data['tags'])))
        f.write(content)

        print(target_file)

    if time_aware:
        update_at = arrow.get(data['updatedAt'])
        update_at_epoch = int(update_at.strftime('%s'))
        os.utime(target_file, (update_at_epoch, update_at_epoch))
        stat = os.stat(target_file)


def process_file(source, output, folder_map, old_name, notes_dir):
    data = text_to_dict(read_file(source))
    write_boostnote_markdown(data, output, folder_map, old_name, notes_dir)


def main(boostnote_dir, output):
    """
    :input: input folder path
    :output: output folder path
    """
    folder_map = read_folder_names(os.path.join(boostnote_dir, 'boostnote.json'))
    notes_dir = os.path.join(boostnote_dir, 'notes')
    for name in os.listdir(notes_dir):
        if not name.endswith('.cson'):
            continue

        source = os.path.join(notes_dir, name)
        process_file(source, output, folder_map, name, boostnote_dir)


if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser(
        description="convert boostnote cson format data to markdown")

    parser.add_argument(
        '-s',
        '--source',
        type=str,
        help="directory store the cson files",
        default=".")
    parser.add_argument(
        '-o', '--output', type=str, help="output directory", default="output")

    args = parser.parse_args()

    main(args.source, args.output)
