from threading import Lock


class Session():
    handlers = dict()
    errors = dict()

    def __init__(self, id, client):
        self.id = id
        self.client = client
        self.op_count = 0
        self.lock = Lock()

    def op_id(self):
        with self.lock:
            self.op_count += 1

        return self.op_count

    def op(self, d):
        d['session'] = self.id
        d['id'] = self.op_id()
        d['nrepl.middleware.caught/print?'] = 'true'
        d['nrepl.middleware.print/stream?'] = 'true'
        return d

    def output(self, x):
        self.client.recvq.put(x)

    def send(self, op, handler=None):
        op = self.op(op)

        if not handler:
            handler = self.client.recvq.put

        self.handlers[op['id']] = handler
        self.client.sendq.put(op)

    def handle(self, response):
        id = response.get('id')
        handler = self.handlers.get(id, self.client.recvq.put)

        try:
            handler.__call__(response)
        finally:
            if response.get('status') == ['done']:
                self.handlers.pop(id, None)
                self.errors.pop(id, None)

    def denounce(self, response):
        id = response.get('id')

        if id:
            self.errors[id] = response

    def is_denounced(self, response):
        return response.get('id') in self.errors

    def terminate(self):
        self.client.halt()