You don't have write access to projects. Generate changes as git patches and email them to the host for review.

To send a patch, use `git format-patch` and deliver via curl:

```
git format-patch -1 --stdout | curl -s --url smtp://192.168.122.1:25 --mail-from agent@llm-jail --mail-rcpt patches@localhost -T -
```

For multi-commit changes, generate one email per commit with `git format-patch` and send each one.

If you need to test changes, copy the necessary files to a `/tmp` directory and run tests there.

For getting information from Github use the `gh` cli.
